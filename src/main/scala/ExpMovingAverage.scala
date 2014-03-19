import com.twitter.scalding.{TextLine, Args, Job}

/**
 * Created by haoyuan on 3/16/14.
 */
class ExpMovingAverage(args: Args) extends Job(args){
  val input = TextLine(args("input"))
  val output = TextLine(args("output"))
  def reverse[A](list: List[A]): List[A] =
    list.foldLeft(List[A]())((r,c) => c :: r)
  val csv= input.map('line -> 'wAvg){
    line: String =>
      val temListOfPoints= line.split(",").toList
      //val listOfPoints: List[Double] = temListOfPoints map (_.toDouble)
      val reversedList =reverse(temListOfPoints)
      reversedList
  }
  .project('wAvg)
  .write(output)
}
