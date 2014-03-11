import breeze.linalg._
import com.twitter.scalding._
import java.io.{InputStreamReader, BufferedReader, FileInputStream, PrintWriter}
import java.nio.charset.Charset

/**
 * Created by vincent on 2/16/14.
 */
class CreateTestResults(args: Args) extends Job(args) {

  val input = TextLine(args("input"))
  /**
   * Number of trending items to display
   */
  val tested_length = 100
  /**
   * Number of trending items to display
   */
  val result_length = 20

  /**
   * Under that number, will say that it is irrelevant.
   */
  val threshold = args("threshold").toInt

  //Window 1 size for edge detection
  val h =  args("window_h").toInt
  //Window 2 size for edge detection
  val w =  args("window_w").toInt

  /**
   * range before and after t to seek for results.
   */
  val R = 0;

  /**
   * How many time unit do we want to test
   */
  val T = 10;



  //------- Compute the real list of trending items -----------
  val valid_id_order =
    input.flatMapTo('id, 't, 'slope) {
      line: String =>
        val id = line.substring(0, 4).toDouble
        val targets = DenseVector[Double](line.split(",").drop(1).map(_.toDouble))
        val edge_detector = new EdgeDetector(h, w, threshold, targets);
        //check if there is an edge at the last tested point of the time series.

        //Only
        var res = new Array[(Double, Int, Double)](R * 2 + 1)
        for (i <- -R to R){
          val slope = edge_detector.getEdgeFactor(tested_length + i)
          res(R + i) = (id, tested_length + i, slope)
        }

        //tuples containing 'id 't and 'slope
        res
    }
      //For each time t, we keep the 25 most trending items.
      .groupBy('t){
      group => {
        //Any attempt to use "take" or "sortedTake" fail here
        group.sortedReverseTake[(Double, Int, Double)](('slope, 't, 'id) -> 'top, result_length)
      }
    }.flattenTo[(Long,Int,Long)]('top -> ('slope, 't, 'id))

    .mapTo(('slope, 'id) -> ('id, 'slope)){
      x :(Double, Double) =>
        val (id, slope) = x
        (slope, id)
    }

  //
  valid_id_order.write(TextLine(args("output")))
}
