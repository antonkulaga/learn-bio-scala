package org.denigma.bio.lessons.peptides

import breeze.linalg.Axis._1
import me.shadaj.genalgo.Resources
import me.shadaj.genalgo.codontable.StandardTable
import me.shadaj.genalgo.sequences.{Protein, AminoAcid}

import scala.annotation.tailrec

object ProteinWeights {

  val table: Map[AminoAcid, Int] = Resources.protein_mass_table.map { s =>
    val split = s.split(' ')
    AminoAcid.fromChar(split(0).head) -> split(1).toInt
  }.toMap

  //lazy val reverseTable = table.map{case (key,value)=>value->key}
  def massesOf(protein:Protein): IndexedSeq[Int] = protein.map(weightOf)


  def weightOf(acid: AminoAcid): Int = table(acid)

  def weightOf(protein: Protein): Int = massesOf(protein).sum


  @tailrec def candidateProteins(masses:List[Int],acc:Seq[Protein] = Seq.empty):Seq[Protein] = masses match {
    case Nil=> acc
    case list=>
      table.filter{ case (key,value)=> value==masses.head} match {
        case  v if v.isEmpty => Seq.empty[Protein]
        case aminos =>
          val acc2 =if(acc.isEmpty) aminos.keys.map(key=>Protein(key)).toSeq else acc.flatMap {
            case prot => aminos.keys.map(k => prot :+ k)
          }
          candidateProteins(masses.tail,acc2 )
      }
  }

  lazy val heaviest: Int = table.values.max

  lazy val lightest: Int = table.values.min

}

