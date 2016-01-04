package so.modernized.whip

import cc.factorie.app.chain.LiteChainModel
import cc.factorie.la.{Tensor1, DenseTensor2, Tensor}
import cc.factorie.model.Weights
import cc.factorie.util.{ArrayDoubleSeq, ArrayIntSeq, IntSeq, DoubleSeq}
import cc.factorie.variable.{DiscreteDomain, DiscreteVar}

import scala.util.Random

/**
 * @author johnsullivan
 */
class HybridChainModel[Y <: DiscreteVar](val domain:DiscreteDomain, val featureDomain:DiscreteDomain, val embeddingSize:Int, feats:Y => (Tensor1, Tensor1)) extends LiteChainModel[Y] {

  def nodeFeatures(node: Y, y: Y#Value): Tensor = {
    val (sparseFeats, embedding) = feats(node)
    val t = ImmutableConcatTensor1(sparseFeats, embedding)
    t outer y
  }

  val nodeWeights: Weights = Weights(new DenseTensor2(featureDomain.size + embeddingSize, domain.size))

  // same as basic model
  val edgeWeights: Weights = Weights(new DenseTensor2(domain.size, domain.size))
  def edgeFeatures(left: Y, right: Y, y1: Y#Value, y2: Y#Value): Tensor = y1 outer y2

}

case class ImmutableConcatTensor1(t1:Tensor1, t2:Tensor1) extends Tensor1 {

  lazy val tuple = t1 -> t2

  lazy val dim1 = t1.dim1 + t2.dim1

  lazy val activeDomain: IntSeq = new ArrayIntSeq(t1.activeDomain.toArray ++ t2.activeDomain.toArray)

  lazy val activeDomainSize: Int = activeDomain.length

  def update(i: Int, v: Double): Unit = throw new NotImplementedError("Not a mutable Tensor")

  lazy val isDense: Boolean = t1.isDense && t2.isDense

  def apply(i:Int):Double = if (i < t1.dim1) {
    t1(i)
  } else if (i < t2.dim1) {
    t2(i)
  } else {
    throw new IndexOutOfBoundsException("%d is beyond max index of %d".format(i, dim1))
  }

  def forallActiveElements(f: (Int, Double) => Boolean): Boolean =
    t1.forallActiveElements(f) && t2.forallActiveElements(f)


  def dot(ds: DoubleSeq): Double = {
    assert(ds.length == dim1)
    t1.dot(new ArrayDoubleSeq(ds.asArray.slice(0, t1.dim1))) +
    t2.dot(new ArrayDoubleSeq(ds.asArray.slice(t1.dim1, dim1)))
  }

  def +=(i: Int, incr: Double): Unit = ???
  def zero(): Unit = ???
  def =+(a: Array[Double], offset: Int, f: Double): Unit = ???

  def indexOf(d: Double): Int = {
    var res = t1.indexOf(d)
    if(res == -1) {
      res = t2.indexOf(d)
    }
    res
  }

  def twoNormSquared: Double = t1.twoNormSquared + t2.twoNormSquared

  def oneNorm: Double = t1.oneNorm + t2.oneNorm

  def max: Double = math.max(t1.max, t2.max)

  def containsNaN: Boolean = t1.containsNaN || t2.containsNaN

  /** Return the values as an Array[Double].  Guaranteed to be a copy, not just a pointer to an internal array that would change with changes to the DoubleSeq */
  def toArray: Array[Double] = t1.toArray ++ t2.toArray

  def infinityNorm: Double = ???

  def maxIndex: Int = t2.maxIndex

  def min: Double = math.min(t1.min, t2.min)

  def sum: Double = t1.sum + t2.sum

  // For proportions; implemented here so that different Tensor subclasses can override it for efficiency
  def sampleIndex(normalizer: Double)(implicit r: Random): Int = if(r.nextBoolean()) {
    t1.sampleIndex(normalizer)(r)
  } else {
    t2.sampleIndex(normalizer)(r)
  }

  def contains(d: Double): Boolean = t1.contains(d) || t2.contains(d)

  def foreachActiveElement(f: (Int, Double) => Unit) {
    t1.foreachActiveElement(f)
    t2.foreachActiveElement(f)
  }

  def maxIndex2: (Int, Int) = t2.maxIndex2
}