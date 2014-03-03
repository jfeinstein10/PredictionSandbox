import breeze.linalg.DenseVector

/**
 * Created by jeremy on 3/3/14.
 * https://github.com/conversocial/velocity-monster/blob/master/src/main/scala/com/conversocial/bursts/ExponentialSmoothing.scala
 */
class TripleExponentialModel(alpha: Double, beta: Double, gamma: Double, period: Integer) extends IterativeModel {

  require(alpha >= 0 && alpha <= 1)
  require(beta >= 0 && beta <= 1)
  require(gamma >= 0 && gamma <= 1)

  override def forecast(x: DenseVector[Double], n: Int): DenseVector[Double] = {
    DenseVector[Double](forecast(x.toArray, n))
  }

  def forecast(x: Array[Double], n: Int): Array[Double] = {
    require(x.length > 0)
    require(x.length % period == 0)
    require(x.length / period >= 2)
    require(0 <= n && n <= period)

    val seasons = x.grouped(period)

    val smoothing = Array.fill(x.length)(0.0)
    val trendSmoothing = Array.fill(x.length)(0.0)
    val seasonalSmoothing = Array.fill(x.length)(0.0)
    val forecast = Array.fill(x.length + n)(0.0)

    smoothing(1) = x(0)
    trendSmoothing(1) = {
      val first :: second :: _ = seasons.sliding(2).next()
      (first, second).zipped.map(_ - _).sum / (period * period)
    }

    val seasonalIndices = {
      val averages = seasons.map(
        season => season.sum / season.length
      )
      val observations = seasons.zip(averages).flatMap({
        case (season, average) =>
          season.map(value => value / average)
      })
      observations.grouped(period).map(
        phases => phases.sum / phases.length
      )
    }
    seasonalIndices.zipWithIndex.foreach({
      case (value, i) =>
        seasonalSmoothing(i) = value
    })

    x.zipWithIndex.drop(2).foreach({
      case (value, i) =>
        smoothing(i) = {
          (1.0 - alpha) * (smoothing(i - 1) + trendSmoothing(i - 1)) +
            (if (i >= period) {
              alpha * value / seasonalSmoothing(i - period)
            } else {
              alpha * value
            })
        }

        trendSmoothing(i) = gamma * (smoothing(i) - smoothing(i - 1)) + (1.0 - gamma) * trendSmoothing(i - 1)

        if (i >= period)
          seasonalSmoothing(i) = beta * value / smoothing(i) + (1.0 - beta) * seasonalSmoothing(i - period)

        if (i + n >= period)
          forecast(i + n) = (smoothing(i) + n * trendSmoothing(i)) * seasonalSmoothing(i - period + n)
    })

    forecast
  }

}
