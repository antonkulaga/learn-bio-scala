package org.denigma.bio.kmers

import scala.collection.mutable
import scala.collection.mutable.HashMap


object Mismatches extends Basic{


  /**
   * Bruteforce hack to mutate generate mutated kmers
   * @param str
   * @return
   */
  def mutate(str: String): Set[String] =     str.indices.flatMap { idx =>
      Set(str.updated(idx, 'A'), str.updated(idx, 'T'), str.updated(idx, 'G'), str.updated(idx, 'C'))
    }.toSet


  def mismatches(str: String, d: Int): Set[String] =  (str, d) match {
      case (s, 0) => Set(s)
      case (s, n) => mutate(s) flatMap(mismatches(_, n-1))
    }



  def hammingDistance(first:String,second:String): Int = {
    first
      .zip(second).map { case (one, two) => if (one == two) 0 else 1}
      .foldLeft(0)((acc,el)=>acc+el)+Math.abs(first.length-second.length)
  }



  def withMismatchers(key:String,m:Int):Set[String] =  mismatches(key,m)+key


  /**
   *
   * @param text dna
   * @param k kmer length
   * @param m number of mistaches
   * @return
   */
  def frequencies(text:String, k:Int,m:Int, withReverse:Boolean = false) =
  {
    import collection.mutable.HashMap
    val kmers = new HashMap[String,List[Int]] //place to store kmers positions

    def addKmer(k:String,index:Int) =  kmers.get(k) match {
        case Some(list) =>
          kmers.update(k, index :: list)
        case None =>
          kmers += (k -> List(index))
    }
    text.sliding(k).zipWithIndex.foreach{  case (key,index)=>
      this.withMismatchers(key,m).foreach{case r=>
        addKmer(r,index)
        if(withReverse) addKmer(this.reverseComplement(r),index)
      }

    }
    kmers
  }

  /**
   *
   * @param text
   * @param k Kmer length
   * @param m number of mismatches
   * @param withReverse take into consideration reverse strange
   * @return Iterable with most frequent kmers with mismatches
   */
  def mostFrequent(text:String,k:Int,m:Int, withReverse:Boolean = false): mutable.Iterable[String] = {
    val res = this.frequencies(text,k,m,withReverse)
    val max = res.values.maxBy(v=>v.size).size
    res.collect{case (key,value) if value.size==max=>key}
  }


}
