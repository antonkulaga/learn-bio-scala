package org.denigma.bio.lessons.peptides

import me.shadaj.genalgo.codontable.StandardTable
import me.shadaj.genalgo.sequences.{AminoAcid, Protein}
import org.denigma.bio.lessons.peptides.sequencer.{SimpleCycloSequencer, LinearSequencer, CycloSequencer}
import org.denigma.bio.lessons.peptides.spectrum.{LinearSpectrum, Convolution}

object Quizes {

  def specFromStr(spec:String) = spec.split(' ').map(v=>v.toInt)

  def conv(spec:Seq[Int]) = {
    object Conv extends Convolution
    val res = Conv.getConvolution(spec).groupBy(c=>c).toSeq.sortWith((a,b)=>a._2.size>b._2.size)
    print(res)
  }
  //conv(specFromStr("0 57 118 179 236 240 301"))


  def linearScoreFromSpec(spec:Seq[Int], p:Protein) = {
    val ms = ProteinWeights.massesOf(p)
    val cs = CycloSequencer(spec)
    val s= cs.linearScore(ms)
    println(p)
    println(s)
  }

  val prots = Seq(
    strToProtein("QCV"),
    strToProtein("TVQ"),
    strToProtein("CTV"),
    strToProtein("VAQ"),
    strToProtein("AQV"),
    strToProtein("AVQ")
  )
  //peptideConsistentWithLinearSpectrum(specFromStr("0 71 99 101 103 128 129 199 200 204 227 230 231 298 303 328 330 332 333"),prots)

  //scoreFromSpec(specFromStr("0 71 71 71 131 131 131 156 198 199 199 202 202 202 333 333 333 404 404"),  strToProtein("MAMA"))
  //linearScoreFromSpec(specFromStr("0 97 129 129 129 194 226 323 323 355 452"),strToProtein("PEEP"))

  def peptideConsistentWithLinearSpectrum(spe:Seq[Int],prots:Seq[Protein])  = {
    val ss = LinearSequencer(spe)
    val pvs = prots.map(p=>p->ProteinWeights.massesOf(p)).toMap
    val res = pvs.collect{case (key,value) if LinearSpectrum(value).value.forall(ss.spectrum.contains)=>key}.toList
    println(res.mkString(" "))
  }


  def scoreFromSpec(spec:Seq[Int], p:Protein) = {
    val ms = ProteinWeights.massesOf(p)
    val cs = CycloSequencer(spec)
    val s= cs.score(ms)
    println(p)
    println(s)
  }

  def peptidesFromLinearSpectrum(spe:Seq[Int]) = {
    val ss = LinearSequencer(spe)
    val result = ss.bestMatches
    val prots = result.flatMap(r=>ProteinWeights.candidateProteins(r.toList))
    println(prots)
  }


  def strToProtein(str:String) = Protein(str.map(s=>AminoAcid.fromChar(s)))

  //peptideConsistentWithLinearSpectrum(specFromStr("0 71 99 101 103 128 129 199 200 204 227 230 231 298 303 328 330 332 333"),Seq("AQV", "TCE", "TCQ",  "CET",  "CTV",  "CTQ").map(s=>strToProtein(s)))



  //peptidesFromCycloSpectrum(specFromStr("0 71 101 113 131 184 202 214 232 285 303 315 345 416"))

  def peptidesFromCycloSpectrum(spe:Seq[Int]) = {
    val ss = SimpleCycloSequencer(spe)
    val result = ss.bestMatches
    val prots = result.flatMap(r=>ProteinWeights.candidateProteins(r.toList))
    println(prots)
  }


  def proteinDNANumber(prot:Protein) = {
    val num = prot.foldLeft(1)( (acc,el)=> acc * StandardTable.codonsForAmino(el).size   )
    println(num)
  }

}
