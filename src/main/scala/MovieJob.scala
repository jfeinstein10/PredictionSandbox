import breeze.linalg._
import com.twitter.scalding._
import nak.regress._
import scala.math.pow

/**
 * Created by jeremy on 2/8/14.
 */
class MovieJob(args: Args) extends Job(args) {

  val input = TextLine(args("input"))
  val output = TextLine(args("output"))

  val n = 10 // number of points
  val d = 2 // degree of the polynomial
  val arr = DenseVector.zeros[Double](n)
  for (i <- 0 to n-1) arr(i) = i

  /**
   * Input, x values (time)
   */
  var X = DenseVector.horzcat(DenseVector.ones[Double](n),  arr)
  for (i <- 2 to d) {
    X = DenseMatrix.horzcat(X, arr.map(pow(_,i)).toDenseMatrix.t)
  }

  /**
   * Lambda is a parameter. The higher it is, less likely the matrix will overfit data (smoother curve).
   * Overfit is likely when you do not have a lot of data and your curve degree (length of w) is high.
   */
  val lambda = 1.0
  val lambdaMatrix = DenseMatrix.eye[Double](d) * lambda

  val csv = input
    .flatMap('line -> 'slope) {
    line: String =>
      val target = DenseVector[Double](line.split(",").slice(0, n).map(_.toDouble))
      val result = X \ target
      println(result)
      result.toArray
  }
    .project('slope)
    .groupAll { _.sortBy('slope).reverse }
    .write(output)
}
