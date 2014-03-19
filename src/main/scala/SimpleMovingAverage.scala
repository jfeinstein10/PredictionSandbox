import com.twitter.scalding.{TextLine, Job, Args}

/**
 * Created by haoyuan on 3/17/14.
 */

/* */
class SimpleMovingAverage (args: Args) extends Job(args) {
  val input = TextLine(args("input"))
  val output = TextLine(args("output"))
  val window =10 //window size of simple moving average
  val csv=input.map('line->'sAvg){
      line : String =>
        val temListOfPoints= line.split(",").toList
        val listOfPoints: List[Double] = temListOfPoints map (_.toDouble)
        val result = movingAverage(listOfPoints,window)
        result
    }
  .project('sAvg)
  .write(output)

  def movingAverage(values: List[Double], period: Int): List[Double] = {
    //getting the first average
    val first = (values take period).sum / period
    //Now we make two lists. First, the list of elements to be subtracted.
    //Next, the list of elements to be added:
    val subtract = values map (_ / period)
    val add = subtract drop period
    //We can add these two lists by using zip.
    // This method will only produce as many elements as the smaller list has,
    //which avoids the problem of subtract being bigger than necessary:
    val addAndSubtract = add zip subtract map Function.tupled(_ - _)
    //We finish by composing the result with a fold:
    val res = (addAndSubtract.foldLeft(first :: List.fill(period - 1)(0.0)) {
      (acc, add) => (add + acc.head) :: acc
    }).reverse
    res
  }


}
