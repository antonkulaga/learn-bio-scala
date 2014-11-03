package org.denigma.bio.kmers
import scala.collection.immutable._

object Skew extends Basic{
  import org.denigma.bio.kmers.{Mismatches, Clumps}

  import scala.collection.immutable.{IndexedSeq, WrappedString}


//  def skew(str:String):Int = str.count(_ == 'G') - str.count(_ == 'C')
//
//  def skewRange(str:String,range:Range): IndexedSeq[Int] = for {
//    a <- range
//    i =  a % str.length
//  } yield skew(str.take(i))

  def skewChar(ch:Char) = ch match {
    case 'G' => 1
    case 'C' => -1
    case _=> 0
  }

  def skewReverse(str:String): List[Int] = str.foldLeft(0->List(0)){
    case ((max,acc),el)=>
      val m = this.skewChar(el)+max
      (m,m::acc)
  }._2

  def skew(str:String, i:Int, min:Int = 0):Int = if(i<=min) 0 else this.skew(str,i-1,min) + skewChar( str( (i -1) % str.length))

  //def skewReverse(str:String, i:Int,min:Int = 0):List[Int] = if(i<min) Nil else skew(str,i,min)::skewReverse(str,i-1,min)

  def skewIndexed(str:String) = skewReverse(str).reverse.zipWithIndex//.minBy{case (el,i)=>el}._1

  def minSkew(ind:List[(Int,Int)]) = ind.minBy{case (el,i)=>el}._1

  def minIndexes(ind:List[(Int,Int)]): List[Int] = {
    val m = minSkew(ind)
    ind.collect{case (el,i) if el==m=>i}
  }

}
