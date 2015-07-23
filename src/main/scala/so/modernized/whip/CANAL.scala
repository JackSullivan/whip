package so.modernized.whip

import scala.collection.mutable
import scala.reflect.ClassTag

class RangeSeqOrdering[A,C <% Ordered[C]](getOrdered:A => C) extends Ordering[Seq[A]] {
  private def getBounds(s:Set[A]) = {
    val sq = s.toSeq.map(getOrdered).sorted
    sq.head -> sq.last
  }
  override def compare(x: Seq[A], y: Seq[A]) = {
    val xStart = getOrdered(x.head)
    val xEnd = getOrdered(x.last)
    val yStart = getOrdered(y.head)
    val yEnd = getOrdered(y.last)
    if(xEnd < yStart) {
      -1
    } else if (yEnd < xStart) {
      1
    } else if (xStart.compare(yStart) == 0 && xEnd.compare(yEnd) == 0) {
      0
    } else {
      throw new IllegalArgumentException("Orderable Range Seqs must not overlap")
    }
  }
}

/**
 * Takes a distance measure on pairs and orders them by the size of that measure
 */
class PairDistanceOrdering[A, C <% Ordered[C]](dist:((A, A) => C)) extends Ordering[(A, A)] {
  override def compare(x: (A, A), y: (A, A)): Int = dist.tupled(x) compare dist.tupled(y)
}

trait CANAL {

  // when implemented, this should be memoized for speed
  def simLink(v1s:Iterable[Vertex], v2s:Iterable[Vertex]):Double
  def deltaMeasure(grouping:Iterable[Seq[Vertex]]):Double
  def mu(d:Double, dm1:Double) = (d - dm1)/dm1

  def run2[C <% Ordered[C] : ClassTag](g:Graph, numCats:Int, getOrdered:(Vertex => C)): Seq[C] = {
    def updateSimlinkPairs(grouping:Seq[(Seq[Vertex], Int)]) = grouping.sliding(2).collect{ case Seq(pair1, pair2) => pair1 -> pair2 }.toSeq.sortBy { case ((v1, _), (v2, _)) => simLink(v1, v2) }

    var currentGrouping = g.vertices.groupBy(getOrdered).values.toSeq.map(_.toSeq).sorted(new RangeSeqOrdering(getOrdered)).zipWithIndex
    var remainingPairs = updateSimlinkPairs(currentGrouping)

    var deltaPminusOne = null.asInstanceOf[Double]
    var deltaP = deltaMeasure(currentGrouping.map(_._1))

    val numLeaves = currentGrouping.length
    var currentIdx = numLeaves
    val merges = new Array[(Int, Int, Double)](numLeaves -1)
    val mergeHeap = new mutable.PriorityQueue[(Int, Int, Double)]()(Ordering.by({tup:(Int, Int, Double) => tup._3}))
    val subclusters = new Array[Seq[Vertex]]((numLeaves * 2) - 1)
    currentGrouping foreach {case (v, i) => subclusters(i) = v}

    while(remainingPairs.nonEmpty) {
      val ((m1, idx1), (m2, idx2)) = remainingPairs.head
      currentGrouping = currentGrouping.sliding(2).collect {
        case Vector(v1, v2) if v1 != m1 && v1 != m2 => v1
        case Vector(v1, v2) if v1 == m1 =>
          val parentIdx = currentIdx
          currentIdx += 1
          val newCluster = m1 ++ m2
          subclusters(parentIdx) = newCluster
          newCluster -> parentIdx
      }.toSeq
      remainingPairs = updateSimlinkPairs(currentGrouping)

      deltaPminusOne = deltaP
      deltaP = deltaMeasure(currentGrouping.map(_._1))

      mergeHeap.enqueue((idx1, idx2, mu(deltaP, deltaPminusOne)))
    }

    val cutoffs = (0 until numCats).map { _ =>
      val (v1, _, _) = mergeHeap.dequeue()
      getOrdered(subclusters(v1).last)
    }
    cutoffs
  }
}

trait Edge {
  def to:Vertex
  def from:Vertex
}
trait Vertex {
  def edges:Iterable[Edge]
}
trait Graph {

  def vertices:Iterable[Vertex]

}

class Dendrogram[A](nodes:Array[Seq[A]]) {

  private val numNodes = (nodes.length * 2) - 1
  private val merges = new Array[(Int, Int, Double)](nodes.length - 1)
  private var currentIdx = nodes.length

  def nextId = currentIdx

  def mergeId(n1:Int, n2:Int, distance:Double):Int = {
    val parentId = currentIdx
    currentIdx += 1
    merges(parentId - nodes.length) = (n1, n2, distance)
    parentId
  }

  //@inline
  //def children(parent:Int):Seq[A] = allNodes(parent)._1

  //@inline
  //def index(a:Seq[A]):Int = nodeIdxMap(a)

  def toDotString(composeString:Set[A] => String = _.mkString(", ")):String = {
    val sb = new StringBuilder("digraph Dendrogram {\n")
    val edgeString = "\ti%s -> i%s [label=\"%.2f\"];\n"
    val leafString = "\ti%s [label=\"%s\"];\n"

    merges.zipWithIndex foreach { case t@((c1, c2, score), idx) =>
      val parent = idx + nodes.length
      sb append edgeString.format(c1, parent, score)
      sb append edgeString.format(c2, parent, score)
    }
    sb.append("\n")
    nodes.zipWithIndex foreach { case (elem, idx) =>
        sb append leafString.format(idx, composeString(elem))
    }
    sb.append("}")
    sb.toString()
  }
}

object Dendrogram {
  def main(args:Array[String]): Unit = {
    val dend = new Dendrogram(Array("a", "b", "c", "d", "e", "f"))

    dend.mergeId(0,1,0.1)
    dend.mergeId(2,3,0.3)
    dend.mergeId(7,4,0.6)
    dend.mergeId(6,5,0.7)
    dend.mergeId(9,8,0.8)

    //println(dend.toDotString)
    println(dend.groupings(3))
    //println(dend.children(8))
  }
}
