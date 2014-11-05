import me.shadaj.genalgo.sequences.DNA
import me.shadaj.genalgo.uniprot.Uniprot
import org.biojava.bio.proteomics.StructureTools
import org.biojava3.core.sequence.DNASequence
import org.biojava3.core.sequence.transcription.TranscriptionEngine
import org.denigma.bio.kmers.{Clumps, Skew, Mismatches}
import org.denigma.bio.lessons.orig.Leven
import scala.concurrent.duration._
import scala.concurrent.Await
import scala.concurrent.duration.Duration._

val one = "CAGAAAGGAAGGTCCCCATACACCGACGCACCAGTTTA"
val two = "CACGCCGTATGCATAAACGAGCCGCACGAACCAGAGAG"
val three= "CAGAAAGGAAGGTCCCCATACACCGACGCACCAGTTTA"
Mismatches.hammingDistance(one,two)

//reverse complement
DNA("TTGTGTC").reverseComplement


//skew indexed
val sk = "GATACACTTCCCAGTAGGTACTG"
val ski = Skew.skewIndexed(sk)
ski.maxBy{case (sk,index)=> sk}

//clumps number
val cls = "GCACAAGGCCGACAATAGGACGTAGCCTTGAAGACGACGTAGCGTGGTCGCATAAGTACAGTAGATAGTACCTCCCCCGCGCATCCTATTATTAAGTTAATT"
Clumps.clumps(cls,4,30,3) mkString " "

//search with mismatches
//wr
val string = "TACGCATTACAAAGCACA"
string.sliding(2).count(el=>el.contains("A"))
  //.reduce(_+_)
val kmer = "TGCA"
val dnSize = Mismatches.mismatches(kmer,3).size

Leven.distance("kitten", "sitting")
Leven.distanceFun("kitten","sitting")


val f = Uniprot.getFasta("P27925")

val creb = Await.result(f,1 second)

creb.sequence.map(p=>println(p.long))

import org.biojava.bio

val dna = new DNASequence("ATG")
val engine = TranscriptionEngine.getDefault() //Get the default engine
val rna = engine.getDnaRnaTranslator().createSequence(dna)
//val protein = engine.getRnaAminoAcidTranslator().createSequence(rna);

import scala.collection.JavaConversions._

//Or to jump to it straight away use this method (coming soon)
val protein = engine.translate(dna)
protein.getAsList.foreach(c=>println(c.getLongName))
