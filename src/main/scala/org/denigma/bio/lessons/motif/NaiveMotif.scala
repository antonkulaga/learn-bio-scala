package org.denigma.bio.lessons.motif

import org.denigma.bio.kmers.Mismatcher

import scala.collection.mutable


object NaiveMotif extends Mismatcher
{

  def motifs(k:Int,d:Int,seqs:Seq[String]): Set[String] = {
    import collection.mutable.HashMap
    val kmers: Seq[String] = seqs.flatMap(_.sliding(k))
    (for {
      km <- kmers
      pat <- this.withMismatchers(km, d)
      strs = this.withMismatchers(pat,d)
      if seqs.forall(s => strs.exists(str => s.contains(str)))
    } yield pat).toSet //.flatten
  }

  def maxScore(samples:Int,length:Int) = {
    ( 3* samples / 4 /*- samples % 4*/) * length
  }


//  def motifs(k:Int,d:Int,seqs:Seq[String]) = {
//    import collection.mutable.HashMap
//    val kmers: Seq[String] = seqs.flatMap(_.sliding(k))
//    val counts = new HashMap[String,Int] //place to store kmers positions
//    for {
//      km <- kmers
//      str <- this.mismatches(km,d)
//    } if(counts.contains(str)) counts.update(str,counts(str)+1) else counts +=(str->1)
//  }
//
//  for {
//    km <- kmers
//    strs = this.withMismatchers(km, d)
//    if seqs.forall(s => strs.exists(str => s.contains(str)))
//  } yield km //.flatten

}