package so.modernized.whip.info

import scala.collection.mutable

object Entropy {

  def empiricalProb[A](iter:Iterator[A]):Map[A,Double] = {
    val m = new mutable.HashMap[A, Double]().withDefaultValue(0.0)
    var denom = 0.0
    while(iter.hasNext) {
      val datum = iter.next()
      m(datum) = m(datum) + 1
      denom += 1
    }
    m.mapValues(_ / denom).toMap
  }

  def log2(e:Double) = if(e == 0.0) 0.0 else math.log(e)/math.log(2)

  def discreteEntropy[A](data:Iterator[A]):Double = {
    val iter = empiricalProb(data).iterator
    var res = 0.0
    while(iter.hasNext) {
      val (_, prob) = iter.next()
      res += (prob * log2(prob))
    }
    -res
  }

  def mSpacingEntropy[Num](data:Seq[Num])(implicit num:Numeric[Num]) = {
    import num._
    var res = 0.0
    val n = data.size
    val m = math.round(math.sqrt(n)).toInt
    val s = data.sorted
    val iter = (0 until n-m).iterator
    while(iter.hasNext) {
      val idx = iter.next()
      res += log2((1.0 + n)/m * (s(idx + m) - s(idx)).toDouble())
    }
    res
  }

  def cardinalityRatioProminence[A](iter:Iterator[A], width:Int) = {
    var datasetSize = 0
    val domain = mutable.HashSet[A]()
    while(iter.hasNext) {
      domain += iter.next()
      datasetSize += 1
    }
    if(datasetSize == domain.size || domain.size == 1) 0.0 else (datasetSize.toDouble/(domain.size * width)) - 6
  }
}
