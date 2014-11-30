import breeze.generic.{MappingUFunc, UFunc}
import breeze.linalg.operators.OpAdd
import breeze.optimize.GradientTester
import breeze.optimize.linear.LinearProgram
import me.shadaj.genalgo.sequences._
import org.denigma.bio.kmers.{Mismatcher, Mismatches}
import org.denigma.bio.lessons.motif.{Scoring, NaiveMotif}

//NaiveMotif.maxScore(10,15)
Scoring.log(1000,10)
import org.denigma.bio.linear.BioMatrix._
import org.denigma.bio.linear.{ entropy}
import breeze.linalg._
val t = DenseVector[String]("a","b","c")
val t2 = DenseVector[String]("x","y","z")
val tc = DenseVector.vertcat(t,t2)
val st1 = DenseMatrix(
 ("a","b","c"),
 ("q","w","e"),
 ("x","y","z")
)
val num = DenseMatrix.ones[Double](30,40)
val st2 = st1 + st1
//st1.apply(*, ::)
//val pmat = DenseMatrix(
//"0.2 0.2 0 0 0 0 0.9 0.1 0.1 0.1 0.3 0".split(" ").map(_.toDouble),
//"0.1 0.6 0 0 0 0 0 0.4 0.1 0.2 0.4 0.6".split(" ").map(_.toDouble),
//"0 0 1 1 0.9 0.9 0.1 0 0 0 0 0".split(" ").map(_.toDouble),
//"0.7 0.2 0 0 0.1 0.1 0 0.5 0.8 0.7 0.3 0.4".split(" ").map(_.toDouble))
//distribute(pmat)
//val byRaw: BroadcastedRows[DenseMatrix[Double], DenseVector[Double]] = pmat(*,::)
//val byCol: BroadcastedColumns[DenseMatrix[Double], DenseVector[Double]] = pmat(::,*)
//val cs = entropy(byCol)
//entropy(DenseVector(0.2, 0.6, 0.0, 0.2))
//entropy(DenseVector(0.0, 0.6, 0.0, 0.4))
//entropy(DenseVector(0.0, 0.0, 0.9, 0.1))
val d = DenseMatrix(
  (1.0,2.0,3.0),
  (4.0,1.0,2.0),
  (1.0,2.0,4.0)
)
import org.denigma.bio.linear._
counts(d)
val d2 = DenseMatrix(
  Seq(A,T,G),
  Seq(G,T,A),
  Seq(C,T,C)
)
counts(d2,Seq(A,C,G,T))
counts(d2(::,*),Seq(A,C,G,T))
