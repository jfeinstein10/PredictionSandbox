  import breeze.linalg._
  import com.twitter.scalding._
  import scala.math.pow

  /**
   * Created by jeremy on 2/8/14.
   */
  class TrendDetectionByCurve(args: Args) extends Job(args) {

    val input = TextLine(args("input"))
    val output = TextLine(args("output"))
    /**
     * Number of points in the time serie to use
     */
    val data_length = args("data_length").toInt
    /**
     * Number of trending items to display
     */
    val result_length = args("result_length").toInt

    val curve_degree = args("curve_degree").toInt

    val curve_overfit_control = args("curve_overfit_control").toDouble


    /**
     * X = Input x values. looks like
     * 1 1 1 ...
     * 1 2 4 ...
     * 1 3 9 ...
     * ....
     */
    val arr = DenseVector.zeros[Double](data_length)
    for (i <- 0 to data_length-1) arr(i) = i
    var X = DenseVector.horzcat(DenseVector.ones[Double](data_length),  arr)
    for (i <- 2 to curve_degree) {
      X = DenseMatrix.horzcat(X, arr.map(pow(_,i)).toDenseMatrix.t)
    }

    /**
     * Lambda is a parameter. The higher it is, less likely the matrix will overfit data (smoother curve).
     * Overfit is likely when you do not have a lot of data and your curve degree (length of w) is high.
     */
    val overfit_control_matrix = DenseMatrix.eye[Double](curve_degree) * curve_overfit_control

    val results =
      input.flatMap('line -> ('id, 'slope))
      { line : String =>
          val id = line.substring(0, 2).toDouble
          val targets = DenseVector[Double](line.split(",").takeRight(data_length).map(_.toDouble))
          println(id)
          Array((id, getTrendingFactor(targets)))
      }
      .project('id, 'slope)
      .groupAll { _.sortBy('slope).reverse }

    //
    results.limit(result_length).write(output)


    /**
     * Match a curve on the time serie. Will return the curve future acceleration.
     * @param targets
     */
    def getTrendingFactor(targets : DenseVector[Double]):Double = {
      //Solve the linear equation
      val weigths = X \ targets
      println("weights : " + weigths)
      println("targets : " + targets)
      println(getAccelerationAt(weigths, data_length + 1))

      return getAccelerationAt(weigths, data_length + 1)
    }

    /**
     * @param function scalar of the function from which we want to get acceleration. Should get a lib for that.
     * @param x point at which we want the acceleration
     */
    def getAccelerationAt(function : DenseVector[Double], x : Double) : Double = {
      val scalars = function.copy

      //Derivate twice to get acceleration..
      for (i <- 2 to curve_degree){
        scalars(i) = i*(i-1)*scalars(i);
      }

      //Calculate value of X a that position
      val terms = DenseVector.zeros[Double](curve_degree )
      for (i <- 2 to curve_degree){
        terms(i-2) = scala.math.pow(x, i-2).*(scalars(i))
      }

      val result = terms.sum
      return result
    }
  }
