import breeze.linalg._

/**
 * Created on 2/19/14 by Vincent
 *
 *
 * @param window_h Size of the window before t
 * @param window_w Size of the window after t
 * @param threshold Minimal value (under which value is irrelevant)
 * @param values Time series to be tested
 */
class EdgeDetector(window_h: Integer, window_w: Integer, threshold: Double, values: DenseVector[Double]) {


  /**
   * Detect edge in time series. Return edge evaluation
   * @param t Time t where edge detection will start
   * @param length Length   of edge detection
   */
  def getEdgeFactorCurve(t: Integer, length: Integer): DenseVector[Double] = {
    //Solve the linear equation
    val results = DenseVector.zeros[Double](length)
    for (i <- 0 to length) {
      results(i) = getEdgeFactor(i + t)
    }
    return results
  }

  /**
   * Detect edge in time series. Return edge evaluation
   * @param t Time t where edge detection will be executed
   */
  def getEdgeFactor(t: Integer): Double = {
    //Solve the linear equation
    val windowH = values.slice(t - window_h, t - 1);
    val windowW = values.slice(t, t + window_w);
    //We use "/" and not "-" so trending is proportionnal.
    val h_average = applyThreshold(windowH.sum / window_h)
    val w_average = applyThreshold(windowW.sum / window_w)

    val result = w_average - h_average
    return result
  }

  def applyThreshold(value: Double): Double = {
    if (value < threshold) {
      return threshold.toDouble
    }
    return value
  }
}
