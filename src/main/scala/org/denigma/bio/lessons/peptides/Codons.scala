package org.denigma.bio.lessons.peptides

import me.shadaj.genalgo.codontable.StandardTable
import me.shadaj.genalgo.sequences._

import scala.annotation.tailrec

object Codons {

  val fromInt = Array(Phe, Leu, Ser, Tyr, Cys, Trp, Pro, His, Gln, Arg, Ile, Met, Thr, Asn, Lys, Val, Ala, Asp, Glu, Gly, Stop)

  val aminos: Map[String, AminoAcid] = fromInt.map(b => b.name->b ).toMap



  /**
   * Takes
   * @param Str string of aminos in three letters format
   *            like "Val-Lys-Leu-Phe-Pro-Trp-Phe-Asn-Gln-Tyr"
   * @param split
   * @return
   */
  def aminosFrom3(str:String,split:String="-") = str.split(split).map(aminos)
  def codonsCombination3(str:String) = aminosFrom3(str).foldLeft(1)( (acc,el)=>acc*StandardTable.codonsForAmino(el).size)



}


