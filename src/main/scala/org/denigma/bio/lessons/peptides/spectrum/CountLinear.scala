package org.denigma.bio.lessons.peptides.spectrum

/**
 * Just coutns the number of all possible spectrum combinations for linear peptide
 */
object CountLinear {
  def cutsNum(num:Int,len:Int) = num-len+1

  def allCuts(num:Int) = (for(i <- 1 to num) yield cutsNum(num,i)).sum +1


}
