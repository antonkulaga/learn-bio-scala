package org.denigma.bio.linear

import breeze.generic.UFunc
import breeze.linalg.sum._
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.{OpArray, ValuesVisitor}
import me.shadaj.genalgo.sequences.{RNABase, DNABase}

import scala.collection.mutable



class DistVisitor[Element] extends ValuesVisitor[Element]{

  val counts = mutable.HashMap.empty[Element,Int]
  var count = 0

  def visit(a: Element): Unit = {
    if(counts.contains(a)) counts.update(a,counts(a)+1) else counts(a) = 1
    count+=1
  }

  def zeros(numZero: Int, zeroValue: Element): Unit = {
    //sum += zeroValue * numZero
  }
  def result = counts.values.map(v=>v/count)
}


object entropy extends UFunc {

  class LogVisitor extends ValuesVisitor[Double]{
    private val _l2 = Math.log(2)

    def log(x:Double, base:Double) = Math.log(x) / Math.log(base)

    def log2(x:Double) = Math.log(x) / _l2

    var sum: Double = 0.0

    def visit(a: Double): Unit = {
      if(a!=0.0) sum -= a* this.log2(a)
    }

    def zeros(numZero: Int, zeroValue: Double): Unit = {
      //sum += zeroValue * numZero
    }
  }


  implicit def reduce[T](implicit iter: CanTraverseValues[T, Double]): Impl[T, Double] = new Impl[T, Double]
  {

    def apply(v: T): Double = {

      val visit = new LogVisitor
      iter.traverse(v, visit)
      visit.sum
    }
  }

}
