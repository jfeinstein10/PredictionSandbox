import breeze.linalg.DenseVector

/**
 * Created by jeremy on 2/24/14.
 */

object ARIMATest {

  def main(args: Array[String]) {
    val ar = Array[Double](1)
    val ma = Array[Double]()
    val d = 0
    val intercept = 0
    val model = new ARIMAModel(ar, ma, d, intercept)
    val x = DenseVector[Double](1, 2, 3, 4, 5)
    val z = model.forecast(x, 5)
    println(z.data.mkString(","))
  }

}
