import breeze.linalg.DenseVector

/**
 * Created by jeremy on 2/26/14.
 * Brown's linear exponential smoothing as found on:
 * http://people.duke.edu/~rnau/411avg.htm
 */
class BrownsExponentialModel(alpha:Double) extends IterativeModel {

  override def forecast(x: DenseVector[Double], n: Int): DenseVector[Double] = {
    if (x.length < 2)
      throw new IllegalArgumentException()
    val predictions = DenseVector.zeros[Double](x.length+n)
    predictions(0) = x(0)
    predictions(1) = x(0)
    val errors = DenseVector.zeros[Double](x.length+n)
    errors(0) = 0
    errors(1) = predictions(1) - x(1)

    for (i <- 2 until predictions.length) {
      predictions(i) = 2*predictions(i-1) - predictions(i-2) - 2*(1-alpha)*errors(i-1) + Math.pow(1-alpha, 2)*errors(i-2)
      errors(i) = if (i < x.length) (x(i) - predictions(i)) else 0
    }

    return predictions.slice(x.length, predictions.length)
  }

}
