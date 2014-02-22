import breeze.linalg.{DenseMatrix, DenseVector}

/**
 * Created by jeremy on 2/9/14.
 */
class PolynomialModel(x:DenseVector[Double], y:DenseVector[Double], degree:Int) extends TimeBasedModel(x, y) {

  var X = DenseVector.ones[Double](x.length).asDenseMatrix.t
  for (i <- 1 to degree) {
    X = DenseMatrix.horzcat(X, x.map(math.pow(_,i)).asDenseMatrix.t)
  }
  val coeff = X \ y

  override def forecast(time: Double): Double = {
    var prediction = coeff(0)
    for (i <- 1 to degree) {
      prediction += coeff(i)*math.pow(time,i)
    }
    prediction
  }

}
