package EdgeDetection;
import breeze.linalg._
import com.twitter.scalding._

/**
 * Created by root on 14-02-19.
 */
public class EdgeDetector {
    /**
     * Detect edge in time series. Return edge evaluation
     * @param targets
     */
    def getTrendingFactor(targets: DenseVector[Double]): Double = {
        //Solve the linear equation
        val windowH = targets.slice(0, h);
        val windowW = targets.slice(h + 1, h + w);
        //We use / and not - so trending is proportionnal.
        val haverage = applyThreshold(windowH.sum / h)
        val waverage = applyThreshold(windowW.sum / w)

        val result = waverage / haverage
        return result
    }

    def applyThreshold(value: Double): Double = {
        if (value < threshold) {
            return threshold.toDouble
        }
        return value
    }
}
