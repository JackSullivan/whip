package so.modernized.whip

import scala.collection.mutable

/**
 * This iterator takes an iterator[A] an aggregation to combine As to form a B, and cuttoff that takes
 * current aggregate's states and returns true if it is time to start a new aggregate
 */
class AggregatingIterator[A, B](iter:Iterator[A], agg:Seq[A] => B, cutoff:(A,A) => Boolean) extends Iterator[B] {
  println("agg iter instantiated")
  private val bIter = iter.buffered
  println("buffered iter instantiated")

  override def hasNext: Boolean = bIter.hasNext

  def next(): B = {
    val toAggregate = mutable.ArrayBuffer[A]()
    var previousVal = null.asInstanceOf[A]
    while(previousVal == null || (iter.hasNext && (!cutoff(previousVal, bIter.head)))) {
      previousVal = bIter.next()
      println("Adding " + previousVal)
      toAggregate += previousVal
    }
    agg(toAggregate)
  }
}
