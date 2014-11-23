package org.denigma.bio.lessons.peptides.sequencer

import org.denigma.bio.lessons.peptides.ProteinWeights
import org.denigma.bio.lessons.peptides.spectrum.{LinearSpectrum, CycloSpectrum}

import scala.annotation.tailrec


class AbstractCycloSequencer(spec:Seq[Int],possibleMasses:Seq[Int] = ProteinWeights.table.values.toSeq)
{
  val spectrum = spec.sorted

  lazy val maxMass= spectrum.max

  lazy val  masses:Seq[Int] = possibleMasses.distinct//possibleMasses.filter(spectrum.contains).distinct

  lazy val start:Seq[Seq[Int]] = masses.map(m=>Seq(m))


  def expand(peptides:Seq[Seq[Int]]): Seq[Seq[Int]] = peptides.flatMap(p=>masses.map(m=> p :+ m ) /*++masses.map(m=> m +: p )*/)


}


class LeaderSequencer(spec:Seq[Int],possibleMasses:Seq[Int] = ProteinWeights.table.values.toSeq) extends AbstractCycloSequencer(spec,possibleMasses)
{
  lazy val spectrumCounts: Map[Int, Int] = spectrum.groupBy(v=>v).map{case (key,value)=>key->value.size}//.toMap

  def score(peptide:Seq[Int]) = CycloSpectrum(peptide).value.groupBy(v=>v).foldLeft(0)
  { case (acc,(key,list))=> if(spectrumCounts.contains(key)) acc+Math.min(list.size,spectrumCounts(key)) else acc }

  def linearScore(peptide:Seq[Int]) = LinearSpectrum(peptide).value.groupBy(v=>v).foldLeft(0)
  { case (acc,(key,list))=> if(spectrumCounts.contains(key)) acc+Math.min(list.size,spectrumCounts(key)) else acc }

  def linearLeaderboard(peptides:Seq[Seq[Int]]): Seq[(Int, Seq[Int])] = peptides.collect{case p if p.sum<=this.maxMass=>
    linearScore(p)->p}.sortWith((a,b) => a._1 > b._1)

  def cyclicLeaderboard(peptides:Seq[Seq[Int]]): Seq[(Int, Seq[Int])] = peptides.collect{case p if p.sum<=this.maxMass=>
    score(p)->p}.sortWith((a,b) => a._1 > b._1)


  /**
   * Takes top n elelements together with their tiers
   * @param sortedLeaderboard
   * @param n number of top elements that together with tiers will be taken
   * @tparam T
   * @return
   */
  def trimWithScore[T](sortedLeaderboard: Seq[(Int, T)] ,n:Int):Seq[(Int,T)] =   if(sortedLeaderboard.isEmpty) Seq.empty[(Int,T)]  else {
    val nsc: Int = if(n-1>=sortedLeaderboard.length) sortedLeaderboard.last._1 else sortedLeaderboard(n-1)._1
    sortedLeaderboard.takeWhile{ case (score,pep) => score>=nsc}
  }
  def trim[T](sortedLeaderboard: Seq[(Int, T)] ,n:Int): Seq[T] =   this.trimWithScore(sortedLeaderboard,n).map(_._2)
}
