package org.denigma.bio.kmers

trait Basic {

  def reverseComplement(str:String): String = str.reverse.map{
    case 'A'=>'T'
    case 'T'=>'A'
    case 'G'=>'C'
    case 'C'=>'G'
  }
}
