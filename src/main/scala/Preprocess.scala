import com.twitter.scalding._
import java.lang.System
import scala.collection.mutable.ListBuffer

/**
 * Created by jeremy on 3/7/14.
 */
class Preprocess(args: Args) extends Job(args) {

  val output = TextLine(args.getOrElse("output", "output"))

  val defaultStart = System.currentTimeMillis / 1000
  val defaultWindowSize = "hour"
  val defaultNumWindows = 20
  val defaultAction = "view"

  val startTime = args.getOrElse("startTime", defaultStart.toString).toInt
  val windowSizeArg = args.getOrElse("windowSize", defaultWindowSize.toString)
  val numWindows = args.getOrElse("numWindows", defaultNumWindows.toString).toInt
  val actionArg = args.getOrElse("action", defaultAction.toString)

  // the number of seconds in each of the following
  val windowSize = windowSizeArg match {
    case "hour" => 3600
    case "day" => 86400
    case "week" => 604800
    case "year" => 31536000
    case _ => -1 
  }
  if (windowSize < 0)
    throw new IllegalArgumentException("Invalid windowSize argument")

  val endTime = startTime - windowSize*numWindows

  def randomInt(mn:Int, mx:Int): Int = {
  	scala.util.Random.nextInt(mx-mn+1)+mn
  }
  def generateU2i(n:Int) = {
    val list = ListBuffer[(Int, Int, Int)]()
  	for (i <- 0 to n)
      list += new Tuple3(randomInt(0, 10), randomInt(10, 20), randomInt(endTime, startTime))
    list.toList
  }

  val list = generateU2i(10000)
  // TODO perform a CPU version to check

  // now do the scalding  version
  val source = IterableSource(list, ('user, 'item, 'time))
  source.groupBy('item) {
    _.foldLeft('time -> 'timeseries)(Array.fill[Int](numWindows)(0)) {
      (seriesSoFar:Array[Int], time:Int) =>
        seriesSoFar(((time-endTime)/windowSize).toInt) += 1
        seriesSoFar
    } 
  }.map('timeseries -> 'timestring) {
    timeseries:Array[Int] =>
      timeseries.mkString(",")
  }.project('item,'timestring)
    .write(output)
}
