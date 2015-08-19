package so.modernized.whip

import spire.math._
import spire.algebra._
import spire.implicits._
import scala.{specialized => sp}

class TaylorKDE[@sp(Double, Float) N : Field : Trig : NRoot](xs:Array[N], p:Natural, numClusters:Int) {

  val sortedXs = xs.qsorted

  val max = sortedXs(xs.length-1)
  val min = sortedXs(0)
  val res = (max - min)/numClusters

  val ls = (0 until numClusters).map { idx =>
    val lower = idx * res
    val upper = lower + res
    Interval(lower, upper)
  }

  def midpoint[Num : Field](i:Interval[Num]):Num = i match {
    case All() => Field[Num].zero
    case Bounded(lower, upper, _) => (upper - lower)/2.0
  }


  def factorial(n:Natural):Natural = {
    var res = Natural.one
    cfor(Natural.zero)(_ < n, _ + 1) { n_i =>
      res *= n_i
    }
    res
  }

  cfor(Natural.zero)(_ < p, _ + 1) { k =>

    val fac = (2 ** k)/factorial(k)
    cfor(0)(_ < numClusters, _ + 1) { l =>
      val x_l = midpoint(ls(l))

    }

  }
}

class NaiveTaylorKDE(xs:Array[Double], p:Int, numClusters:Int, sigma:Double) {
  val n = xs.length
  val sortedXs = xs.sorted
  val ls = {
    val intSize = (sortedXs(xs.length) - sortedXs(0))/numClusters
    (0 until numClusters).map { i =>
      val lower = i * intSize
      val upper = lower + intSize
      (lower, upper, (upper - lower)/2)
    }
  }

  val clusterAssignments = {
    val ca = new Array[Double](n)
    var i = 0
    var j = -1
    var upper = Double.MinValue
    var midpoint = null.asInstanceOf[Double]
    while(i < xs.length) {
      val x_i = sortedXs(i)
      if(x_i > upper) {
        j += 1
        (_, upper, midpoint) = ls(j)
      }
      ca(i) = midpoint
      i += 1
    }
    ca
  }

  def factorial(i:Int) = (1 to i).product

  (0 until p).map { k =>
    val fac = math.pow(2,k)/factorial(k)
    val b_kl = sortedXs.zip(clusterAssignments).map { case(x_i, x_c) =>
      1/(math.sqrt(2 * math.Pi) * n * sigma) *
        math.exp((-math.pow(math.abs(x_i - x_c), 2))/math.pow(sigma,2)) *
        math.pow((x_i - x_c)/sigma, k)
    }.sum
  }


}


