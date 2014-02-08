import breeze.linalg.DenseMatrix
import com.twitter.scalding.Args

/**
 * Created by jeremy on 2/8/14.
 */
object Main {
  def main(args: Array[String]): Unit = {
    val args = new Args(Map())
    val job = new MovieJob(args)
  }
}
