import breeze.linalg.DenseVector

/**
 * Created by jeremy on 3/3/14.
 * https://github.com/nchandra/ExponentialSmoothing/blob/master/src/main/java/com/nc/tsa/HoltWinters.java
 * https://github.com/optimistoolkit/optimistoolkit/blob/master/OptimisY3/TrustFramework/TrustFrameworkService/IPTrustFramework/src/main/java/eu/optimis/tf/ip/service/operators/ExponentialSmoothingAggregator.java
 */
class HoltWintersModel(alpha:Double, beta:Double, gamma:Double, period:Integer) extends IterativeModel {

  require(alpha >= 0 && alpha <= 1)
  require(beta >= 0 && beta <= 1)
  require(gamma >= 0 && gamma <= 1)

  override def forecast(x: DenseVector[Double], n: Int): DenseVector[Double] = {

    require(x.length % period == 0)
    require(x.length / period >= 2)
    require(n <= period)

    val seasons = x.length/period
    val a0 = calculateInitialLevel(x)
    val b0 = calculateInitialTrend(x)
    val initialSeasonalIndices = calculateSeasonalIndices(x, seasons)

    val St = DenseVector.zeros[Double](x.length)
    val Bt = DenseVector.zeros[Double](x.length)
    val It = DenseVector.zeros[Double](x.length)
    val Ft = DenseVector.zeros[Double](x.length + n)

    // Initialize base values
    St(1) = a0
    Bt(1) = b0

    for (i <- 0 until period) {
      It(i) = initialSeasonalIndices(i)
    }

    // Start calculations
    for (i <- 2 until x.length) {

      // Calculate overall smoothing
      if ((i - period) >= 0) {
        St(i) = alpha * x(i) / It(i - period) + (1.0 - alpha) * (St(i - 1) + Bt(i - 1))
      } else {
        St(i) = alpha * x(i) + (1.0 - alpha) * (St(i - 1) + Bt(i - 1))
      }

      // Calculate trend smoothing
      Bt(i) = gamma * (St(i) - St(i - 1)) + (1 - gamma) * Bt(i - 1)

      // Calculate seasonal smoothing
      if ((i - period) >= 0) {
        It(i) = beta * x(i) / St(i) + (1.0 - beta) * It(i - period)
      }

      // Calculate forecast
      if (((i + n) >= period)) {
        Ft(i + n) = (St(i) + (n * Bt(i))) * It(i - period + n)
      }
    }
    Ft
  }

  def calculateInitialLevel(x: DenseVector[Double]): Double = {
    x(0)
  }

  def calculateInitialTrend(x: DenseVector[Double]): Double = {
    var sum = 0d
    for (i <- 0 until period)
      sum += x(period+i) - x(i)
    sum/(period*period)
  }

  def calculateSeasonalIndices(x: DenseVector[Double], seasons:Integer): DenseVector[Double] = {
    val seasonalAverage = DenseVector.zeros[Double](seasons)
    val seasonalIndices = DenseVector.zeros[Double](period)

    val averagedObservations = DenseVector.zeros[Double](x.length)

    for (i <- 0 until seasons) {
      for (j <- 0 until period) {
        seasonalAverage(i) += x((i * period) + j)
      }
      seasonalAverage(i) /= period
    }

    for (i <- 0 until seasons) {
      for (j <- 0 until period) {
        averagedObservations((i * period) + j) = x((i * period) + j) / seasonalAverage(i)
      }
    }

    for (i <- 0 until period) {
      for (j <- 0 until seasons) {
        seasonalIndices(i) += averagedObservations((j * period) + i)
      }
      seasonalIndices(i) /= seasons
    }

    seasonalIndices
  }

}
