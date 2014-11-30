package org.denigma.bio.lessons.motif

object Scoring
{
  def log(x:Int, base:Int) =
  {
    Math.log(x) / Math.log(base)
  }


  def entropy(seq:Seq[Int]) = -seq.foldLeft(0.0)(
    (acc,el)=> acc  + el * this.log(el,2)
  )

}
