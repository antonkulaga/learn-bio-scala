package org.denigma.bio.lessons.peptides

import me.shadaj.genalgo.sequences.AminoAcid
import me.shadaj.genalgo.Resources
import me.shadaj.genalgo.codontable.StandardTable
import org.denigma.bio.lessons.peptides._
import me.shadaj.genalgo.sequences._
import org.denigma.bio.lessons.peptides._
import me.shadaj.genalgo.codontable._

import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeq

object LinearSpectrum {
  def apply(protein:Protein):LinearSpectrum =  LinearSpectrum(ProteinWeights.massesOf(protein))

  def apply(proteinStr:String):LinearSpectrum =  this.apply(Protein.apply(proteinStr))
}

case class LinearSpectrum(peptide:Seq[Int]){

  lazy val value: IndexedSeq[Int]  =  0 +: (for {
    len <- 1 to peptide.length
    index <- 0 to peptide.length - len
  } yield peptide.slice(index, index+len).sum) :+ peptide.sum

}


object CycloSpectrum{

  def apply(protein:Protein):CycloSpectrum =  CycloSpectrum(ProteinWeights.massesOf(protein))

  def apply(proteinStr:String):CycloSpectrum =  this.apply(Protein.apply(proteinStr))

}

case class CycloSpectrum(peptide: Seq[Int])
{

 lazy val value: IndexedSeq[Int]  =  0 +: (for (index <- 0 until peptide.length; len <- 1 until peptide.length) yield  subpeptide(index, len).sum ) :+ peptide.sum

  def subpeptide(from:Int,len:Int) = {
    val to = from+len
    val toNorm = to % peptide.length
    if(toNorm<=from) peptide.take(toNorm) ++ peptide.drop(from) else peptide.slice(from,to)
  }

}


/**
 * Just coutns the number of all possible spectrum combinations for linear peptide
 */
object CountLinear {
  def cutsNum(num:Int,len:Int) = num-len+1

  def allCuts(num:Int) = (for(i <- 1 to num) yield cutsNum(num,i)).sum +1


}
