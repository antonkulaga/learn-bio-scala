package org.denigma.bio.crispr

import java.io.File
import java.net.URI

import org.biojava3.core.sequence.io.GenbankReaderHelper

import scala.io.Source


object GenBank
{

  def loadProtein(filePath:String) = GenbankReaderHelper.readGenbankProteinSequence(new File(filePath))

  def loadDNA(filePath:String) =  GenbankReaderHelper.readGenbankDNASequence(new File(filePath))

}

object FastaReader {

  def read(id:String) =  Source.fromURI(new URI(s"http://www.uniprot.org/uniprot/$id.fasta"))

}