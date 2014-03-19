import com.twitter.scalding._
import breeze.linalg._
import scala.runtime.ScalaRunTime._
/**
 * Created by haoyuan on 2/21/14.
 * this section of code is calculating the weighted avg. of the first n(avgWindow) data
 */

/*Just calculating the first n datum points*/
class NaiveWeightedMovingAvg(args: Args) extends Job(args) {
  val input = TextLine(args("input"))
  val output = TextLine(args("output"))
  val avgWindow=5 //number of points used to calculate average
  val weights= DenseVector[Double](0.2,0.2,0.2,0.2,0.2)
  val csv = input.map('line -> 'wAvg){
    line: String =>
      val points = DenseVector[Double](line.split(",").slice(0,avgWindow).map(_.toDouble))
      val result = points:*weights
      val resSum=breeze.linalg.sum(result)
      resSum
  }
  .project('wAvg)
  .write(output)





}
