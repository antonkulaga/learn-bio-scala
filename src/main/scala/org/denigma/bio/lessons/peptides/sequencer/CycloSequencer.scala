package org.denigma.bio.lessons.peptides.sequencer

import org.denigma.bio.lessons.peptides.ProteinWeights
import org.denigma.bio.lessons.peptides.spectrum.{LinearSpectrum, CycloSpectrum}

import scala.annotation.tailrec
import scala.language.postfixOps


object CycloSequencer {

  def fromMasses(str:String): CycloSequencer = CycloSequencer(str.split(" ").map(_.toInt))

  def fromMasses(masses:Seq[Int]): CycloSequencer = CycloSequencer(masses)

  def apply(spec:Seq[Int],possibleMasses:Seq[Int] = ProteinWeights.table.values.toSeq) = new CycloSequencer(spec,possibleMasses)

}

class CycloSequencer(spec:Seq[Int],possibleMasses:Seq[Int] = ProteinWeights.table.values.toSeq) extends LeaderSequencer(spec,possibleMasses)
{

//  @tailrec final def run(peptides:Seq[Seq[Int]],leadNum:Int = 10, best:Option[(Int, Seq[Int])] = None): Option[(Int,Seq[Int])] = {
//    val pep = this.expand(peptides)
//    cyclicLeaderboard(pep) match {
//      case leaders if leaders.isEmpty=>best
//      case leaders =>
//        run(this.trim(leaders,leadNum), leadNum, best.map(b=> if(b._1>leaders.head._1) b else leaders.head).orElse(leaders.headOption)   )
//    }
//  }


  /**
   * Experimental function that finds many peptides
   * @param peptides
   * @param leadNum
   * @param best
   * @return
   */
  @tailrec final def runMany(peptides:Seq[Seq[Int]],leadNum:Int = 10, best:Seq[(Int,Seq[Int])] = Seq.empty ): Seq[(Int,Seq[Int])]  = {
    val pep = this.expand(peptides)
    cyclicLeaderboard(pep)
    match {
      case leaders if leaders.isEmpty=>best
      case leaders =>
        runMany(this.trim(leaders,leadNum), leadNum, this.trimWithScore( (leaders++best).sortWith((a,b)=>a._1>b._1),1)   )
    }
  }

  def best(leadersNum:Int) = bestMatches(leadersNum).headOption

  def bestMatches(leadersNum:Int) = runMany(start,leadNum = leadersNum)


  def prettyString(peps:Seq[Seq[Int]]) = peps.map(this.prettyProtein).mkString(" ")
  def prettyProtein(pep:Seq[Int]) = pep.mkString("-")



}
