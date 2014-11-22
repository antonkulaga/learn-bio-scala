package org.denigma.bio.lessons.peptides

import me.shadaj.genalgo.sequences.{AminoAcid, Protein}

import scala.collection.immutable.IndexedSeq

object Brute {

  val masses = ProteinWeights.table
  lazy val aminos = ProteinWeights.table.keys.toList


  def findMinMax(mass:Int) = (mass / ProteinWeights.heaviest, mass / ProteinWeights.lightest)

  def numberOfPeptidesForMass(mass:Int): Int = {
    val (minL,maxL) = findMinMax(mass)
    (for{
      i <-minL to maxL
      gen = generate(i,mass)
    } yield gen).sum
  }

  def genListForLen(len:Int) = masses.map(kv=>(1 to len).map(i=>kv)).flatten.toList

  def generate(len:Int,mass:Int): Int = {
    def isOfMass(colb:List[(AminoAcid,Int)]) = colb.foldLeft(0)((acc,el)=>acc+el._2) == mass
    val code: List[(AminoAcid, Int)] = genListForLen(len)
    code.combinations(len).foldLeft(0)((acc,el)=>if( isOfMass(el) ) acc+1 else acc)
      //.collect{case comb if filter(comb.foldLeft(0)((acc,el)=>acc+el._2))=>Protein(comb.map(_._1):_*)}

  }

}
