package so.modernized.whip.testing

import spire.algebra._
import spire.implicits._
import spire.math._

import scala.util.Random

object SpireKDE{
  def apply[@specialized(Double) N : Field : Trig : NRoot](xs:Array[N], x:N, sigma:N) = {
    val a = 1/(sqrt(2 * pi) * sigma * xs.length)
    var b = Field[N].zero
    cfor(0)(_ < xs.length, _ + 1) { i =>
      b = b + exp(-(x - xs(i))/(sigma ** 2))
    }
    a * b
  }
}

object NaiveKDE {
  def apply(xs:Array[Double], x:Double, sigma:Double) =
    (1/(math.sqrt(2 * math.Pi) * sigma)) * xs.map{ x_i =>
      math.exp(-(x - x_i)/math.pow(sigma,2))
    }.sum
}

object FastKDE {
  def apply(xs:Array[Double], x:Double, sigma:Double) = {
    var res = 0.0
    var i = 0
    var x_i = null.asInstanceOf[Double]
    while(i < xs.length){
      x_i = xs(i)
      i += 1
      res += math.exp(-(x - x_i)/math.pow(sigma,2))
    }
    (1/(math.sqrt(2 * math.Pi) * sigma)) * res
  }

}


object KDETest {

  def main(args:Array[String]): Unit = {
    val xs = (1 to 1000).map(_ => Random.nextGaussian()).toArray

    xs foreach { x =>
      NaiveKDE(xs, x, 0.01)
      SpireKDE(xs, x, 0.01)
      FastKDE(xs, x, 0.01)
    }

    var fastTime:Long = 0
    var spireTime:Long = 0
    var naiveTime:Long = 0
    var start = null.asInstanceOf[Long]
    (1 to 100) foreach { run =>
      xs foreach { x =>
        start = System.currentTimeMillis()
        NaiveKDE(xs, x, 0.01)
        naiveTime += (System.currentTimeMillis() - start)
      }
      println("Naive finished %s runs in %.4f secs on average".format(run, (naiveTime/1000.0)/run))
      xs foreach { x =>
        start = System.currentTimeMillis()
        SpireKDE(xs, x, 0.01)
        spireTime += (System.currentTimeMillis() - start)
      }
      println("Spire finished %s runs in %.4f secs on average".format(run, (spireTime/1000.0)/run))
      xs foreach { x =>
        start = System.currentTimeMillis()
        FastKDE(xs, x, 0.01)
        fastTime += (System.currentTimeMillis() - start)
      }
      println("Fast finished %s runs in %.4f secs on average".format(run, (fastTime/1000.0)/run))

    }
    println("Average Speed")
    println("Naive : %.4f secs".format((naiveTime/1000.0)/100))
    println("Spire : %.4f secs".format((spireTime/1000.0)/100))
    println("Fast  : %.4f secs".format((fastTime/1000.0)/100))
  }


}
