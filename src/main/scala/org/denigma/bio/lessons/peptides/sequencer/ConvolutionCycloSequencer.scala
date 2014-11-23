package org.denigma.bio.lessons.peptides.sequencer

import org.denigma.bio.lessons.peptides.ProteinWeights
import org.denigma.bio.lessons.peptides.spectrum.Convolution


object ConvolutionCycloSequencer {

  def fromMasses(str:String): ConvolutionCycloSequencer = new ConvolutionCycloSequencer(str.split(" ").map(_.toInt))

  def fromMasses(masses:Seq[Int]): ConvolutionCycloSequencer =new  ConvolutionCycloSequencer(masses)

}


case class ConvolutionCycloSequencer(spec:Seq[Int], convTop:Int = 20, possibleMasses:Seq[Int] =  Seq.empty/*ProteinWeights.table.values.toSeq*/, minConv:Int = 57,maxConv:Int = 200) extends CycloSequencer(spec,possibleMasses) with Convolution
{

  private lazy val convolutionCounts = this.getConvolution(this.spectrum)
    .filter(p=>p>=minConv && p<=maxConv)
    .groupBy(k=>k).toSeq.map{case (key,list)=>list.size->key}.sortWith(
      (a,b)=>a._1>b._1
    )


  lazy val convolutionLeaders =  this.trim(convolutionCounts,convTop)


  override lazy val  masses:Seq[Int] = (possibleMasses++this.convolutionLeaders).distinct


}
