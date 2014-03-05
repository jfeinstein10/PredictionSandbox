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
  val data_length = args("data_length").toInt

  /**
   * Under that number, will say that it is irrelevant.
   */
  val threshold = args("threshold").toInt

  //Window 1 size
  val h =  args("window_h").toInt
  //Window 2 size
  val w =  args("window_w").toInt


  //------- Compute edges factor
  val valid_id_order =
    input.flatMapTo('id, 'edges) {
      line: String =>
        val id = line.substring(0, 4).toDouble
        val targets = DenseVector[Double](line.split(",").drop(1).map(_.toDouble))

        val edge_detector = new EdgeDetector(h, w, threshold, targets);

        val edges:StringBuffer = new StringBuffer()
        for (i <- h to targets.length - (w)){
          edges.append(",")
          edges.append(edge_detector.getEdgeFactor(i))
        }
        Array((id, edges.toString))
    }
      /*.groupAll {
      _.sortBy('id)

    }*/

  //
  valid_id_order.write(TextLine(args("output")))




}
