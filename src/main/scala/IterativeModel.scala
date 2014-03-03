import breeze.linalg.DenseVector

/**
 * Created by jeremy on 2/26/14.
 */
abstract class IterativeModel {
  def forecast(x:DenseVector[Double], n:Int): DenseVector[Double]
}
