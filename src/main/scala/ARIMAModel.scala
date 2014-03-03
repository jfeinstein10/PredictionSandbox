import breeze.linalg.DenseVector

/**
 * Created by jeremy on 2/11/14.
 * https://github.com/ilagunap/ARIMA-Tools/blob/master/src/arima_tools.C
 */
class ARIMAModel(ar:Array[Double], ma:Array[Double], d:Int, intercept:Double) extends IterativeModel {

  var dSeries:Array[DenseVector[Double]] = Array()

  override def forecast(x:DenseVector[Double], n:Int): DenseVector[Double] = {

    val obs = diff(x)
    val pred = DenseVector.zeros[Double](n)
    val errors = DenseVector.zeros[Double](ma.length)

    // initialize the full window of values
    val win = DenseVector.zeros[Double](obs.length+n)
    val mu = obs.sum/obs.length
    for (i <- 0 until obs.length) win(i) = obs(i)

    for (i <- ar.length until win.length) {

      var new_val = 0.0
      var error_now = 0.0
      var phi_factor = 1.0

      // Do the AR part
      if (ar.length > 0) {
        for (j <- 0 until ar.length) {
          new_val += ar(j) * win(i-1-j)
          phi_factor -= ar(j)
        }
      }

      // Do the MA part
      for (j <- 0 until ma.length)
        new_val += ma(j) * errors(ma.length-1-j)

      // Add intercept
      new_val += intercept * phi_factor

      if (ma.length > 0) {
        // Update predictions
        if (i >= obs.length) {
          win(i) = new_val
          error_now = 0
        } else {
          error_now = win(i) - new_val
        }

        // Update errors
        for (j <- 0 until errors.length-1)
          errors(j) = errors(j+1)
        errors(errors.length-1) = error_now
      } else {
        // Update predictions
        win(i) = new_val
      }
    }

    // pull out the predictions
    for (i <- 0 until n) pred(i) = win(obs.length+i)
    undoDiff(pred)
  }


  def diff(x:DenseVector[Double]): DenseVector[Double] = {
    if (d <= 0)
      return x
    if (d >= x.length)
      throw new IllegalArgumentException()

    // Save original window
    dSeries = dSeries :+ x

    // Differentiate d times
    for (i <- 0 until d) {
      val temp = DenseVector.zeros[Double](dSeries(i).length)
      for (j <- 0 until dSeries(i).length - 1) {
        temp(j) = dSeries(i)(j+1) - dSeries(i)(j)
      }
      dSeries = dSeries :+ temp
    }
    dSeries(d)
  }


  def undoDiff(x:DenseVector[Double]): DenseVector[Double] = {
    if (d <= 0)
      return x

    var diffed = x
    val pred = DenseVector.zeros[Double](x.length)

    // Undo d-differentiated series
    for (i <- 0 until d) {
      for (k <- 0 until x.length) {
        var prev = pred(k)
        if (k == 0) {
          val old = dSeries(dSeries.length-i-2)
          prev = old(old.length-1)
        } else {
          prev = pred(k-1)
        }
        pred(k) = prev + diffed(k)
      }
      diffed = pred
    }
    pred
  }


}
