package org.denigma.bio.linear

import breeze.linalg.operators.{OpSub, OpAdd}
import me.shadaj.genalgo.sequences.{RNA, DNA}


object BioMatrix extends DNAMatrixOps with StringMatrixOps with RNAMatrixOps
{
  def log(x:Int, base:Int) =
  {
    Math.log(x) / Math.log(base)
  }

}

trait DNAMatrixOps {

  implicit object AddDNA2 extends OpAdd.Impl2[DNA, DNA, DNA]
  {
    override def apply(v: DNA, v2: DNA): DNA = v++v2
  }

  implicit object SubDNA2 extends OpSub.Impl2[DNA, DNA, DNA]
  {
    override def apply(v: DNA, v2: DNA): DNA = v.diff(v2)
  }

}

trait StringMatrixOps {
  implicit object AddStr2 extends OpAdd.Impl2[String, String, String]
  {
    override def apply(v: String, v2: String): String = v+v2
  }

  implicit object SubStr2 extends OpSub.Impl2[String, String, String]
  {
    override def apply(v: String, v2: String): String = v.replace(v2,"")
  }

}

trait RNAMatrixOps {

  implicit object AddRNA2 extends OpAdd.Impl2[RNA, RNA, RNA]
  {
    override def apply(v: RNA, v2: RNA): RNA = v++v2
  }

  implicit object SubRNA2 extends OpSub.Impl2[DNA, DNA, DNA]
  {
    override def apply(v: DNA, v2: DNA): DNA = v.diff(v2)
  }

}
