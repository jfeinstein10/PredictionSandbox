/**
 * Created by haoyuan on 2/23/14.
 */


import breeze.linalg.DenseVector
import com.twitter.scalding._

class testbreeze(args: Args) extends Job(args){
  val kernel=DenseVector(1.0,2.0)
  val data=DenseVector(2.0,3.0,4.0,5.0)

}
