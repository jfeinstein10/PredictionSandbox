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
    val data_length = 100
    /**
     * Number of trending items to display
     */
    val result_length = 20

    val curve_degree = args("curve_degree").toInt

    val curve_overfit_control = args("curve_overfit_control").toDouble

    /**
     * Lambda is a parameter. The higher it is, less likely the matrix will overfit data (smoother curve).
     * Overfit is likely when you do not have a lot of data and your curve degree (length of w) is high.
     */
    val overfit_control_matrix = DenseMatrix.eye[Double](curve_degree) * curve_overfit_control

    val results =
      input.flatMap('line -> ('id, 'slope))
      { line : String =>
          val id = line.substring(0, 4).toDouble
          val targets = DenseMatrix(line.split(",").slice(1,data_length + 1).map(_.toDouble)).t
          Array((id, getTrendingFactor(targets.toDenseMatrix)))
      }
      .project('id, 'slope)
      .groupAll { _.sortBy('slope).reverse }

    //
    results.limit(result_length).write(output)



    /**
     * PHI = Input x values. looks like
     * 1 1 1 ...
     * 1 2 4 ...
     * 1 3 9 ...
     * ....
     */
    val arr = DenseVector.zeros[Double](data_length)
    for (i <- 0 to data_length-1) arr(i) = i.toDouble
    var PHI:DenseMatrix[Double] = DenseVector.horzcat(DenseVector.ones[Double](data_length),  arr)
    for (i <- 2 to curve_degree) {
      PHI = DenseMatrix.horzcat(PHI, arr.map(pow(_,i)).asDenseMatrix.t)
    }

    /**
     * Match a curve on the time serie. Will return the curve future acceleration.
     * @param targets
     */
    def getTrendingFactor(targets : DenseMatrix[Double]):Double = {
      //Solve the linear equation
      val Lambda:DenseMatrix[Double] = DenseMatrix.eye[Double](curve_degree + 1) :* curve_overfit_control
      val A:DenseMatrix[Double] =  PHI.t * PHI + Lambda
      val B:DenseMatrix[Double] =  PHI.t * targets

      val weigths =  A \ B
      return getAccelerationAt(weigths, data_length + 1)
    }

    /**
     * @param function scalar of the function from which we want to get acceleration. Should get a lib for that.
     * @param x point at which we want the acceleration
     */
    def getAccelerationAt(function : DenseMatrix[Double], x : Double) : Double = {
      val scalars = function.copy.toDenseVector

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
