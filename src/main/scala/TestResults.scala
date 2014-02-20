import breeze.linalg._
import com.twitter.scalding._

/**
 * Created by jeremy on 2/8/14.
 */
class TestResults(args: Args) extends Job(args) {

  val input = TextLine(args("input"))
  /**
   * Number of points in the time serie to use
   */
  val data_length = args("data_length").toInt
  /**
   * Number of trending items to display
   */
  val result_length = args("result_length").toInt

  /**
   * Under that number, will say that it is irrelevant.
   */
  val threshold = args("threshold").toInt

  //Window 1 size
  val h = data_length
  //Window 2 size
  val w = result_length


  //------- Compute the real list of trending items -----------
  val valid_id_order =
    input.flatMap('line ->('id, 'slope)) {
      line: String =>
        val id = line.substring(0, 2).toDouble
        val targets = DenseVector[Double](line.split(",").takeRight(h + w).map(_.toDouble))
        //Compute average of each window. a better edge detection algorithm could be applied.
        Array((id, getTrendingFactor(targets)))
    }
      .project('id, 'slope, 'num)

      .rename(('num) -> ('original_position))
      .groupAll {
      _.sortBy('slope).reverse
    }

  //
  valid_id_order.limit(result_length)

  /**
   * Detect edge in time series. Return edge evaluation
   * @param targets
   */
  def getTrendingFactor(targets: DenseVector[Double]): Double = {
    //Solve the linear equation
    val windowH = targets.slice(0, h);
    val windowW = targets.slice(h + 1, h + w);
    //We use / and not - so trending is proportionnal.
    val haverage = applyThreshold(windowH.sum / h)
    val waverage = applyThreshold(windowW.sum / w)

    val result = waverage / haverage
    return result
  }

  def applyThreshold(value: Double): Double = {
    if (value < threshold) {
      return threshold.toDouble
    }
    return value
  }


  //------- Compare given results with computed results -----------
  val results_input = TextLine(args("_results_input"))
  val output = TextLine(args("output"))

  val results_evaluation = results_input.flatMap('line -> 'id) {
    line: String =>
      val id = line.substring(0, 3).toDouble
      Array(id)
  }
    .joinWithSmaller('id -> 'id, valid_id_order)
    .mapTo(('num, 'original_position) -> 'distance){
     x :(Integer, Integer) => val distance = scala.math.abs(x._1 - x._2)
  }
    .groupAll {
    _.reduce('distance-> 'total_distance) {
      (total_so_far:Double, distance: Double) => total_so_far + distance
    }
  }
    .flatMap('total_distance -> 'average_distance){
    total_distance : Double =>
      Array(total_distance / result_length)
  }
    .write(output)
}
