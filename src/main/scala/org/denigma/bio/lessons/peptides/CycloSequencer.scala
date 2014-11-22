package org.denigma.bio.lessons.peptides

import scala.annotation.tailrec
import scalaxy.loops._
import scalaxy.streams._
import scala.language.postfixOps

class AbstractCycloSequencer(spec:Seq[Int],possibleMasses:Seq[Int] = ProteinWeights.table.values.toSeq)
{
  val spectrum = spec.sorted

  lazy val maxMass= spectrum.max

  lazy val  masses:Seq[Int] = possibleMasses.distinct//possibleMasses.filter(spectrum.contains).distinct

  lazy val start:Seq[Seq[Int]] = masses.map(m=>Seq(m))


  def expand(peptides:Seq[Seq[Int]]): Seq[Seq[Int]] = peptides.flatMap(p=>masses.map(m=> p :+ m ) /*++masses.map(m=> m +: p )*/)


}

object CycloSequencer {

  def fromMasses(str:String): CycloSequencer = CycloSequencer(str.split(" ").map(_.toInt))

  def fromMasses(masses:Seq[Int]): CycloSequencer = CycloSequencer(masses)

}

case class CycloSequencer(spec:Seq[Int],possibleMasses:Seq[Int] = ProteinWeights.table.values.toSeq) extends AbstractCycloSequencer(spec,possibleMasses)
{

  lazy val spectrumCounts: Map[Int, Int] = spectrum.groupBy(v=>v).map{case (key,value)=>key->value.size}.toMap

  def score(peptide:Seq[Int]) = CycloSpectrum(peptide).value.groupBy(v=>v).foldLeft(0)
    { case (acc,(key,list))=> if(spectrumCounts.contains(key)) acc+Math.min(list.size,spectrumCounts(key)) else acc }

  def linearScore(peptide:Seq[Int]) = LinearSpectrum(peptide).value.groupBy(v=>v).foldLeft(0)
    { case (acc,(key,list))=> if(spectrumCounts.contains(key)) acc+Math.min(list.size,spectrumCounts(key)) else acc }

  def linearLeaderboard(peptides:Seq[Seq[Int]]): Seq[(Int, Seq[Int])] = peptides.collect{case p if p.sum<=this.maxMass=>
    linearScore(p)->p}.sortWith((a,b) => a._1 > b._1)

  def cyclicLeaderboard(peptides:Seq[Seq[Int]]): Seq[(Int, Seq[Int])] = peptides.collect{case p if p.sum<=this.maxMass=>
    score(p)->p}.sortWith((a,b) => a._1 > b._1)


  def trim(sortedLeaderboard: Seq[(Int, Seq[Int])] ,n:Int): Seq[Seq[Int]] =   if(sortedLeaderboard.isEmpty) Seq.empty[Seq[Int]]
  else {
    val nsc: Int = if(n-1>=sortedLeaderboard.length) sortedLeaderboard.last._1 else sortedLeaderboard(n-1)._1
    sortedLeaderboard.collect{ case (score,pep) if score>=nsc =>pep}
  }


  @tailrec final def run(peptides:Seq[Seq[Int]],leadNum:Int = 10, best:Seq[(Int,Seq[Int])] = Seq.empty ): Seq[Seq[Int]]  = {
    val pep = this.expand(peptides)
    cyclicLeaderboard(pep) match {
      case leaders if leaders.isEmpty=>best.map(v=>v._2)
      case leaders =>
        val nextBest:Seq[(Int,Seq[Int])] = if(best.isEmpty) Seq(leaders.head) else if(best.head._1>leaders.head._1) best else Seq(leaders.head) //not optimal but works well
        run(this.trim(leaders,leadNum), leadNum, nextBest)
    }
  }


  def bestMatches(leadersNum:Int): Seq[Seq[Int]] = run(start,leadNum = leadersNum)

  def prettyPrint(peps:Seq[Seq[Int]]) = peps.map(p=>p.mkString("-")).mkString(" ")



}

case class SimpleCycloSequencer(spec:Seq[Int],possibleMasses:Seq[Int] = ProteinWeights.table.values.toSeq) extends AbstractCycloSequencer(spec,possibleMasses)
{

  @tailrec final def run(peptides:Seq[Seq[Int]],solutions:Seq[Seq[Int]] = Seq.empty): Seq[Seq[Int]]  =
    expand(peptides).filter(isInSpectrum)  match {
      case pep if pep.isEmpty =>
        solutions.filter(sol=>CycloSpectrum(sol).value.forall(spectrum.contains))
      case pep =>
        run(pep, solutions ++ pep.filter(p => p.sum == maxMass))
    }



  /**
   * Checks if this peptide is inside spectrum
   * @param peptide
   * @return
   */
  def isInSpectrum(peptide:Seq[Int]) ={
    val sum = peptide.sum
    sum <= maxMass && spectrum.contains(sum)
  }


  lazy val bestMatches = run(start)


  def stringValue = bestMatches.map(r=>r.mkString("-")).mkString(" ")

}