import breeze.linalg.DenseVector

/**
 * Created by jeremy on 2/20/14.
 */
abstract class TimeBasedModel(x:DenseVector[Double], y:DenseVector[Double]) {

  if (x.length != y.length)
    throw new IllegalArgumentException("x and y must have the same length")

  def forecast(time:Double): Double
  def forecast(data:Array[Double]): Array[Double] = data.map(forecast(_))

}
