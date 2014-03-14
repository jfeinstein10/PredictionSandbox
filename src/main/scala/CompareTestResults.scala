import breeze.linalg._
import com.twitter.scalding._
import java.io.{InputStreamReader, BufferedReader, FileInputStream}
import java.nio.charset.Charset

/**
 * Created by vincent on 2/16/14.
 */
class CompareTestResults(args: Args) extends Job(args) {
  /**
   * Number of trending items to display
   */
  val result_length = 10

  //------- Compare given results with computed results -----------
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

  val input_stream_good_results = new FileInputStream(args("test_results"))
  val br_good_result = new BufferedReader(new InputStreamReader(input_stream_good_results, Charset.forName("UTF-8")));
  line = br_good_result.readLine();

  var good_ids = Set[Int]()
  while (line != null){
    good_ids += line.split("\t")(0).toDouble.toInt
    line = br_good_result.readLine()
  }
  br_good_result.close()

  //Comparing the two sets
  var hit_count = 0;
  for (id <- provided_ids){
    if (good_ids.contains(id)){
      hit_count = hit_count + 1
    }
  }
  val score = hit_count.toDouble / result_length * 100;
  println(score + "% of the items were good! " + "element count : " + result_length + ", hits : " + hit_count)



}
