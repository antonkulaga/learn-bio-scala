package org.denigma.bio.kmers

import scala.collection.immutable.HashMap
import scala.collection.mutable

trait  FrequentWords {
  /**
   * Recursive function that counts how often the pattern appears in sequence
   * @param text
   * @param pattern
   * @param startIndex
   * @param count
   * @return
   */
  def patternCount(text:String,pattern:String,startIndex:Int = 0,count:Int = 0):Int = text.indexOf(pattern,startIndex) match {
    case -1=>  count
    case i if i<=text.size-pattern.length => this.patternCount(text,pattern,i+1,count+1)
    case _=>count
  }


  /**
   * Count numbers of kmers with hashmap inside
   * @param text
   * @param n
   * @return
   */
  def counts(text:String,n:Integer) = {
    import collection.mutable.HashMap
    val counts = new HashMap[String,Int]
    text.sliding(n).foreach(key=>counts.get(key) match {
      case Some(value)=>counts.update(key,value+1)
      case None =>counts.+=(key->0)
    })
    counts
  }

  def mostFrequent(text:String, kmer:Integer) = {
    val res = counts(text,kmer)
    val m = res.values.max
    res.collect{   case (key,value) if value==m=>key   }
  }





}



