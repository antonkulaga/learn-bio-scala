package org.denigma.bio.kmers

/**
 * Version with frequency array, not my favourite
 */
object FrequencyArray {

  def frequencies(text:String, k:Int) =  text.sliding(k).map(s=>this.pattern2Number(s,"ACGT"))

  def frequencyArray(text:String, k:Int, dictionary:String = "ACGT") = {
    val power = Math.pow(dictionary.length, k).toInt
    val arr = Array.tabulate(power)(el => 0)
    text.sliding(k) foreach {
      case el =>
        val i = this.pattern2Number(el, dictionary).toInt
        arr(i) = arr(i) + 1
    }

    arr

  }

  /**
   * Generates a reversed list of positions of sequence in the genome
   * NOTE: reverse building is used because it is more efficient to construct lists this way
   * @param text
   * @param pattern
   * @param startIndex
   * @param counts
   * @return
   */
  def patternNumsReverse(text:String,pattern:String,startIndex:Int = 0,counts:List[Int] = List.empty):List[Int] = text.indexOf(pattern,startIndex) match {
    case -1=>  counts
    case i if i<=text.size-pattern.length => patternNumsReverse(text,pattern,i+1,i::counts)
    case _=>counts
  }


  def pattern2Number(pattern:String,dictionary:String):Double= if(pattern.size==0) 0 else
    dictionary.indexOf(pattern.head) match {
      case -1=> throw new NoSuchElementException("such element is not in a dictionary")
      case ind=>
        ind*Math.pow(dictionary.length,pattern.size-1)+pattern2Number(pattern.tail,dictionary)
    }

  def number2Pattern(num:Double,dictionary:String,word:String = ""):String = {
    val r = num % dictionary.length
    val q = (num - r) / dictionary.length
    val l = dictionary(r.toInt)
    if(q.toInt==0) (word+l).reverse else  this.number2Pattern(q,dictionary,word+l)
  }

}
