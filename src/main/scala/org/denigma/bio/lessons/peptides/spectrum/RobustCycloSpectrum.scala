package org.denigma.bio.lessons.peptides.spectrum

import me.shadaj.genalgo.sequences.Protein
import org.denigma.bio.lessons.peptides.ProteinWeights


trait Convolution {

  /**
   * Gets SORTED spectrum
   * @param spectrum
   * @return
   */
  def getConvolution(spectrum:Seq[Int]) = for{
    a <- 0 until spectrum.length-1
    b <- a+1 until spectrum.length
    if spectrum(b)>spectrum(a)
  } yield spectrum(b) - spectrum(a)
}

case class RobustCycloSpectrum(peptide:Seq[Int]) extends CycloSpectrum(peptide) with Convolution{

//  lazy val convolution= for{
//    i <- value
//    b <-value
//    if i > b
//  } yield i -b

  lazy val convolution = this.getConvolution(this.value)
}
