package org.denigma.bio.lessons.peptides.spectrum

import me.shadaj.genalgo.sequences.Protein
import org.denigma.bio.lessons.peptides.{ProteinWeights}

import scala.collection.immutable.IndexedSeq


object CycloSpectrum{

  def apply(protein:Protein):CycloSpectrum =  CycloSpectrum(ProteinWeights.massesOf(protein))

  def apply(proteinStr:String):CycloSpectrum =  this.apply(Protein.apply(proteinStr))

  def apply(peptide: Seq[Int]):CycloSpectrum =  new CycloSpectrum(peptide)


}

class CycloSpectrum(peptide: Seq[Int])
{

  lazy val subpeptides: IndexedSeq[Int] = (for (index <- 0 until peptide.length; len <- 1 until peptide.length) yield  subpeptide(index, len).sum ).sorted

  lazy val value: IndexedSeq[Int]  =  0 +: subpeptides :+ peptide.sum

  def subpeptide(from:Int,len:Int) = {
    val to = from+len
    val toNorm = to % peptide.length
    if(toNorm<=from) peptide.take(toNorm) ++ peptide.drop(from) else peptide.slice(from,to)
  }

}



