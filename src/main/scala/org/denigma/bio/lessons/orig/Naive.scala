package org.denigma.bio.kmers

object Naive {

  /**
   * Naive implementation that is slow but short
   * @param n size of kmer
   * @param genome genome string
   * @return
   */
  def naiveFreqCount(genome:String,n:Integer, inverted:Boolean = false) = {
    val forward = genome.sliding(n,1)
    val all = (if(inverted) forward ++ genome.reverse.sliding(n,1) else forward).toStream
    val ngrams: Map[String, Int] = all.groupBy(el=>el).map{ case (key,value)=>key->value.size}
    val max = ngrams.values.max
    ngrams.filter{case (key,value)=>value==max}.toList
  }

}
