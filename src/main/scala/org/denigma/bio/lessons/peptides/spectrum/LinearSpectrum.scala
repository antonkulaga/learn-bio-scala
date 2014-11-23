package org.denigma.bio.lessons.peptides.spectrum


import me.shadaj.genalgo.sequences.Protein
import org.denigma.bio.lessons.peptides.{ProteinWeights}

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
