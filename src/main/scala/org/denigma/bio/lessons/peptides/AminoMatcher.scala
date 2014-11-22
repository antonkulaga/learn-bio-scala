package org.denigma.bio.lessons.peptides


import me.shadaj.genalgo.codontable.StandardTable
import me.shadaj.genalgo.sequences._

import scala.annotation.tailrec


case class AminoMatcher(protein:Protein){

  type CodonSequence = IndexedSeq[Set[RNA]]

  //lazy val codonsBasesSize = codonsSeq.size*3
  lazy val codonsSeq: CodonSequence =  protein.map(amino=>StandardTable.codonsForAmino(amino).toSet)

  @tailrec final def matcher(rnas:Seq[RNA],codSeq:CodonSequence,acc:String = ""):String =
    if(codSeq.isEmpty) acc
    else if(rnas.size<codSeq.size ) ""
    else if(codSeq.head.contains(rnas.head)) matcher(rnas.tail,codSeq.tail,acc+rnas.head.toString)
    else ""

  def matcher(rna:RNA):String = this.matcher(rna.grouped(3).toSeq , this.codonsSeq)

  def canTranslate(rna:Seq[_]) = rna.length % 3 == 0

  @tailrec final def allSafeMatchers(rnas:Seq[RNA],acc:List[String] = List.empty):List[String] =
    if(rnas.size<codonsSeq.size) acc else this.matcher(rnas,this.codonsSeq) match {
      case ""=> allSafeMatchers(rnas.tail, acc)
      case seq=> allSafeMatchers(rnas.tail,seq::acc)
    }

  def allMatchers(dna:DNA) = dna.toRNA match {
    case rna=> (allSafeMatchers(rna.grouped(3).toSeq)++ allSafeMatchers(rna.tail.grouped(3).toSeq)++allSafeMatchers(rna.tail.tail.grouped(3).toSeq)).map(RNA(_))
  }

  def allMatchersTwoSided(dna:DNA): List[RNA] = allMatchers(dna) ++ allMatchers(dna.reverseComplement).map(_.reverseComplement)
}
