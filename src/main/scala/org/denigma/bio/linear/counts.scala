package org.denigma.bio.linear

import breeze.generic.UFunc
import breeze.linalg.{DenseMatrix, DenseVector, Tensor}
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor

import scala.collection.mutable

object counts extends UFunc {

  //implicit object OpArrayDNA extends OpArray[DNABase]
  //implicit object OpArrayRNA extends OpArray[RNABase]

  class DictionaryVisitor[Element](dict:Seq[Element]) extends Counter[Element]{
    override val counts = mutable.Map(dict.map(d=>d->0):_*)
    def visit(a: Element): Unit = if (counts.contains(a)){
      counts.update(a, counts(a) + 1)
      count += 1
    } else {
      println("not found value in dictionary: "+a.toString)
    }
  }

  class CountVisitor[Element] extends Counter[Element] {
    override val counts:mutable.Map[Element,Int] =  mutable.HashMap.empty[Element, Int]
    def visit(a: Element): Unit = {
      if (counts.contains(a)) counts.update(a, counts(a) + 1) else counts(a) = 1
      count += 1
    }
  }

  abstract class Counter[Element] extends ValuesVisitor[Element]{
    val counts: mutable.Map[Element, Int]
    var count = 0


    def zeros(numZero: Int, zeroValue: Element): Unit = { }
  }

  implicit def reduceWithDict[T,Element](implicit iter: CanTraverseValues[T, Element])= new Impl2[T, Seq[Element], Map[Element,Int]]
  {

    def apply(v: T,dictionary:Seq[Element])= {

      val visit = new DictionaryVisitor[Element](dictionary)
      iter.traverse(v, visit)
      visit.counts.toMap
    }
  }


  implicit def reduce[T,Element](implicit iter: CanTraverseValues[T, Element])= new Impl[T, Map[Element,Int]]
  {

    def apply(v: T)= {

      val visit = new CountVisitor[Element]
      iter.traverse(v, visit)
      visit.counts.toMap
    }
  }
}