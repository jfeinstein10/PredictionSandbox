import breeze.linalg._
import com.twitter.scalding._
import nak.regress._
import scala.math.pow

/**
 * Created by jeremy on 2/8/14.
 */
class MovieJob(args: Args) extends Job(args) {
  TextLine( args("input") )
    .flatMap('line -> 'word) { line : String => tokenize(line) }
    .groupBy('word) { _.size }
    .write( Tsv( args("output") ) )

  // Split a piece of text into individual words.
  def tokenize(text : String) : Array[String] = {
    // Lowercase each word and remove punctuation.
    text.toLowerCase.replaceAll("[^a-zA-Z0-9\\s]", "").split("\\s+")
  }

  /*val n = 10 // number of points
  val d = 2 // degree of the polynomial
  val arr = DenseVector.zeros[Double](n)
  for (i <- 0 to n-1) arr(i) = i

  /**
   * Input, x values (time)
   */
  val X = DenseVector.horzcat(DenseVector.ones[Double](n),  arr)

  /**
   * Input, y values (number of shares, likes, etc.)
   */
  var t =  arr.map(pow(_,2))

  println(t)

  println(X)



  /**
   * Lambda is a parameter. The higher it is, less likely the matrix will overfit data (smoother curve).
   * Overfit is likely when you do not have a lot of data and your curve degree (length of w) is high.
   */
  val lambda = 1.0
  val lambdaMatrix = DenseMatrix.eye[Double](d) * lambda

  println(lambdaMatrix)

  /**
   * Regression
   */
  //val w2 = (lambdaMatrix + X.t.dot(X)) / X.t.dot(t)
  /**
   * Weight vector
   */
  val w2 = LinearRegression.regress(arr.toDenseMatrix, t)
  println(w2)
  /*val csv = TextLine("movies.csv")
    .map('line -> 'slope) {
    line: String =>
      val target = DenseVector[Double](line.split(",").map(_.toDouble))
      val result = LinearRegression.regress(matrix, target)
      println(result)
      result(0)
  }*/*/
}
