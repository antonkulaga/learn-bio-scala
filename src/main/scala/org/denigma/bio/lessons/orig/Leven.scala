package org.denigma.bio.lessons.orig

import scala.annotation.tailrec

object Leven {

  def distance(one:String, two:String)={
    val dist=Array.tabulate(two.length+1, one.length+1){(j,i)=>0}
    for(j<-1 to two.length; i<-1 to one.length)
      dist(j)(i)= if(two(j-1)==one(i-1)) dist(j-1)(i-1) else 1+ Math.min(Math.min(dist(j-1)(i-1), dist(j-1)(i)), dist(j)(i-1))
    dist(two.length)(one.length)
  }


  /*
  WARNING: buggy
   */
  def distanceFun(one:String,two:String): Int = {
    def distanceReverse(one:String,two:String):Int = if(one.isEmpty || two.isEmpty) 0 else
      if(one.head==two.head) distanceReverse(one.tail,two.tail)
        else 1+Math.min(distanceReverse(one.tail,two.tail),Math.min(distanceReverse(one.tail,two),distanceReverse(one,two.tail)))
    distanceReverse(one.reverse,two.reverse)
  }


}
