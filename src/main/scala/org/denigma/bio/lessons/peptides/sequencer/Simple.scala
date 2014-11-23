package org.denigma.bio.lessons.peptides.sequencer

import org.denigma.bio.lessons.peptides.ProteinWeights
import org.denigma.bio.lessons.peptides.spectrum.{CycloSpectrum, LinearSpectrum}

import scala.annotation.tailrec


abstract class BasicSequencer(spec:Seq[Int],possibleMasses:Seq[Int] = ProteinWeights.table.values.toSeq) extends AbstractCycloSequencer(spec,possibleMasses)
{
  def run(peptides:Seq[Seq[Int]],solutions:Seq[Seq[Int]] = Seq.empty): Seq[Seq[Int]]

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
case class LinearSequencer(spec:Seq[Int],possibleMasses:Seq[Int] = ProteinWeights.table.values.toSeq) extends BasicSequencer(spec,possibleMasses)
{
  @tailrec final def run(peptides:Seq[Seq[Int]],solutions:Seq[Seq[Int]] = Seq.empty): Seq[Seq[Int]]  =
    expand(peptides).filter(isInSpectrum)  match {
      case pep if pep.isEmpty =>
        solutions.filter(sol=>LinearSpectrum(sol).value.forall(spectrum.contains))
      case pep =>
        run(pep, solutions ++ pep.filter(p => p.sum == maxMass))
    }

}
case class SimpleCycloSequencer(spec:Seq[Int],possibleMasses:Seq[Int] = ProteinWeights.table.values.toSeq) extends BasicSequencer(spec,possibleMasses)
{

  @tailrec final def run(peptides:Seq[Seq[Int]],solutions:Seq[Seq[Int]] = Seq.empty): Seq[Seq[Int]]  =
    expand(peptides).filter(isInSpectrum)  match {
      case pep if pep.isEmpty =>
        solutions.filter(sol=>CycloSpectrum(sol).value.forall(spectrum.contains))
      case pep =>
        run(pep, solutions ++ pep.filter(p => p.sum == maxMass))
    }


}