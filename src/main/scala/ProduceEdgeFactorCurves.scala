import breeze.linalg._
import com.twitter.scalding._

/**
 * Created by vincent on 2/16/14.
 */
class ProduceEdgeFactorCurves(args: Args) extends Job(args) {

  val input = TextLine(args("input"))
  /**
   * Number of trending items to display
   */
  val tested_length = args("tested_data_length").toInt
  /**
   * Number of trending items to display
   */
  val result_length = args("result_length").toInt

  /**
   * Under that number, will say that it is irrelevant.
   */
  val threshold = args("threshold").toInt

  //Window 1 size
  val h =  args("window_h").toInt
  //Window 2 size
  val w =  args("window_w").toInt


  //------- Compute the real list of trending items -----------
  val valid_id_order =
    input.flatMapTo('id, 'slope) {
      line: String =>
        val id = line.substring(0, 4).toDouble
        val targets = DenseVector[Double](line.split(",").slice(1, tested_length + w + 1).map(_.toDouble))
        val edge_detector = new EdgeDetector(h, w, threshold, targets);
        //check if there is an edge at the last tested point of the time series.
        Array((id, edge_detector.getEdgeFactor(tested_length)))
    }
      .groupAll {
      _.sortBy('slope).reverse

    }

  //
  valid_id_order.limit(result_length).write(TextLine("tmp"))


  //------- Compare given results with computed results -----------
  //This section might need refactor

  val provided_results = TextLine(args("results_input"))
  val computed_results = TextLine("tmp")
  val output = TextLine(args("output"))


}
