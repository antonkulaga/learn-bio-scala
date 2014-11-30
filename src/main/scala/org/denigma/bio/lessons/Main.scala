package org.denigma.bio

import java.io.File

import me.shadaj.genalgo.codontable.StandardTable
import me.shadaj.genalgo.sequences._
import org.denigma.bio.lessons.motif.NaiveMotif
import org.denigma.bio.lessons.peptides._
import org.denigma.bio.lessons.peptides.sequencer.{LinearSequencer, ConvolutionCycloSequencer, SimpleCycloSequencer, CycloSequencer}
import org.denigma.bio.lessons.peptides.spectrum.{LinearSpectrum, CycloSpectrum, Convolution}

import scala.io.Source


object Main extends scala.App{

  val k = 5
  val d = 2
  val seqs = Seq(
    "TAATGTTAAAGGTCTACCTATAGCG",
    "CATTTTAACCCGTGGCCTCACAAAA",
    "TAAATTTCACAATTATAACCACCTT",
    "CTCCGGCGCGTAAGTGGGTAGCTAG",
    "GCAATTAAAGTCGAGTCATTTTAAA",
    "TCTGAGTACCTAAACGATAGTCCTA"
  )

  val motifs = NaiveMotif.motifs(k,d,seqs)
  val result = motifs.mkString(" ")
  println(result)

//  import org.rosuda.REngine
//  import org.rosuda.REngine.Rserve.RConnection
//  val c = new RConnection
//  c.eval("get")
//  val d = c.eval("rnorm(10)")
//  val e = d.asDoubles
//
//  for (i <- 0 until 10) {
//    println(e(i))
//  }

}

