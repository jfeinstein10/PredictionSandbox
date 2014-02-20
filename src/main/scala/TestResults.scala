import breeze.linalg._
import com.twitter.scalding._
import java.io.{InputStreamReader, BufferedReader, FileInputStream, PrintWriter}
import java.nio.charset.Charset

/**
 * Created by vincent on 2/16/14.
 */
class TestResults(args: Args) extends Job(args) {

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
  EvaluateResult()

  def EvaluateResult() {
    val provided_results = args("results_input")

    //Reading provided file.
    val input_stream = new FileInputStream(provided_results)
    val br = new BufferedReader(new InputStreamReader(input_stream, Charset.forName("UTF-8")));
    var i = 0;
    var line = br.readLine();
    var provided_ids =  new Array[Integer](result_length)

    while (line != null){
      provided_ids(i) = line.split("\t")(0).toDouble.toInt
      line = br.readLine()
      i = i + 1

    }
    br.close()

    //Reading temp file where computed results are.

    val input_stream_good_results = new FileInputStream("tmp/part-00000")
    val br_good_result = new BufferedReader(new InputStreamReader(input_stream_good_results, Charset.forName("UTF-8")));
    line = br_good_result.readLine();

    var good_ids = new Array[Integer](result_length)
    i = 0
    while (line != null && i < result_length){
      good_ids(i) = line.split("\t")(0).toDouble.toInt
      line = br_good_result.readLine()
      i = i + 1
    }
    br_good_result.close()

    //Comparing the two sets
    var hit_count = 0;
    for (id <- good_ids ){
      if (provided_ids.contains(id)){
        hit_count = hit_count + 1
      }
    }
    val score = hit_count.toDouble / result_length * 100;
    println(score + "% of the items were good! " + "element count : " + result_length + ", hits : " + hit_count)
  }


}
