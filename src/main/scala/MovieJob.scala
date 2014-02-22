import breeze.linalg._
import com.twitter.scalding._
import nak.regress._
import scala.math.pow

/**
 * Created by jeremy on 2/8/14.
 */
class MovieJob(args: Args) extends Job(args) {

  val input = TextLine(args("input"))
  val output = args.getOrElse("output", "output")

  val d = args.getOrElse("degree", "1").toInt // degree of the polynomial
  val n = args.getOrElse("sample", "20").toInt // the number of points to fit

  val arr = DenseVector.zeros[Double](n)
  for (i <- 0 to n-1) arr(i) = i

  input
    .map('line -> 'coeff) {
    line: String =>
      val y = line.split(",").slice(0, n).map(_.toDouble)
      val model = new PolynomialModel(arr, DenseVector[Double](y), d)
      model.forecast(arr(arr.length-1)+1)
  }
    .project('coeff)
    .groupAll { _.sortBy('coeff).reverse }
    .write(TextLine(output+"-poly"))

  // ARIMA
  input
    .map('line -> ('pred1, 'pred2, 'pred3)) {
    line: String =>
      val y = line.split(",").slice(0, n).map(_.toDouble)
      val model = new ARIMAModel(Array[Double](), Array[Double](), 0, 0)
      val pred = model.forecast(DenseVector[Double](y), 3)
      (pred(0), pred(1), pred(2))
  }
    .project(('pred1, 'pred2, 'pred3))
    .write(TextLine(output+"-arima"))

}
