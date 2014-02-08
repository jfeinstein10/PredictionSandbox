import breeze.linalg._
import com.twitter.scalding._
import nak.regress._
import scala.math.pow

/**
 * Created by jeremy on 2/8/14.
 */
class MovieJob(args: Args) extends Job(args) {
  val n = 10 // number of points
  val d = 2 // degree of the polynomial
  val arr = DenseVector.zeros[Double](n)
  for (i <- 0 to n-1) arr(i) = i
  var matrix = DenseVector.ones[Double](n).toDenseMatrix.t
  for (i <- 1 to d) {
    matrix = DenseMatrix.horzcat(matrix, arr.map(pow(_,i)).toDenseMatrix.t)
  }
  println(matrix)

  val csv = TextLine("movies.csv")
    .map('line -> 'slope) {
    line: String =>
      val target = DenseVector[Double](line.split(",").map(_.toDouble))
      val result = LinearRegression.regress(matrix, target)
      println(result)
      result(0)
  }
}
