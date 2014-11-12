import java.io.File
import java.net.URI

import scala.io.Source

//import org.bionic.crispr._
//import scala.collection.JavaConversions._
//val name = "/home/antonkulaga/denigma/files/pC194 GenBank Sequence.gb"
//
//val vector =  GenBank.loadDNA(name)
//
//vector.foreach{
//  case (key,value)=>
//    val seq = value.toList
//    println(seq.size)
//    println(s"SEQUENCE =  $key \n ${seq.mkString}")
//
//
//}
val path = "/home/antonkulaga/Downloads/"
val table = "RNA_codon_table_1.txt"

val file = Source.fromFile(new File(path+table))

val codons = file.getLines().map(_.split(" "))
codons.toList



