package org.denigma.bio.kmers

object Clumps extends FrequentWords {


  def clumps(text:String, k:Int, interval:Int, minTimes:Int) =
  {
    import collection.mutable.HashMap
    val kmers = new HashMap[String,List[Int]] //place to store kmers positions
    text.sliding(k).zipWithIndex.foreach{ case (key,index)=>
      kmers.get(key) match {
        case Some(list)=>
            kmers.update(key,index::list)
        case None =>kmers +=(key->List(index))
      }
    }

    def inWindow(list:List[Int],acc:Int = 0,prev:Int = -1):Boolean =if(list.isEmpty) false else
    {
      val delta = if(prev == -1) 0 else Math.abs(prev-list.head)
      if ( delta >= interval) inWindow(list.tail,0,list.head)   else  acc+1>=minTimes || inWindow(list.tail, acc+1,list.head)
    }

    kmers.collect{    case (key,value) if inWindow(value.reverse)=>key    }
    //kmers
  }



}
