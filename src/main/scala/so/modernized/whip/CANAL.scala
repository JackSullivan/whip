package so.modernized.whip

import cc.factorie.util.Attr
import org.apache.spark.graphx.Graph
import scala.collection.mutable
import scala.reflect.ClassTag

class OrderableRangeSet[A, C <% Ordered[C]](as:Iterable[A])(getOrdering:A => C) extends Set[A] with Ordered[OrderableRangeSet[A,C]] {

  private var (s, start, end) = as match {
    case set:Set[A] =>
      val sq = set.toSeq.map(getOrdering).sorted
      (set, sq.head, sq.last)
    case sq:Seq[A] =>
      val set = sq.toSet
      val oSeq = sq.map(getOrdering).sorted
      (set, oSeq.head, oSeq.last)
  }

  override def compare(that: OrderableRangeSet[A, C]): Int = if(this.end < that.start) {
    -1
  } else if (this.start > that.end) {
    1
  } else if ((this.start.compare(that.start) == 0) && (this.end.compare(that.end) == 0)) {
    0
  } else {
    throw new IllegalArgumentException("Orderable Range Sets must not overlap")
  }

  private def updateExtrema(newElem:A): Unit = {
    getOrdering(newElem) match {
      case newStart if newStart < start => start = newStart
      case newEnd if newEnd > end => end = newEnd
      case otw => ()
    }
  }

  override def +(elem: A): Set[A] = {
    updateExtrema(elem)
    s + elem
  }
  def ++(elems:OrderableRangeSet[A,C]) = new OrderableRangeSet[A,C](s ++ elems.s)(getOrdering)

  override def contains(elem: A): Boolean = s contains elem
  override def -(elem: A): Set[A] = {
    updateExtrema(elem)
    s - elem
  }
  override def iterator: Iterator[A] = s.iterator
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
  def deltaMeasure(grouping:Iterable[Set[Vertex]]):Double
  def mu(d:Double, dm1:Double) = (d - dm1)/dm1

  def run2[C <% Ordered[C] : ClassTag](g:Graph, numCats:Int, getOrdered:(Vertex => C)): Set[Set[Vertex]] = {
    var currentGrouping = g.vertices.groupBy(getOrdered).values.toSeq.map(vs => new OrderableRangeSet[Vertex, C](vs.toSeq)(getOrdered)).sorted
    val simlinkOrderedvertPairs = currentGrouping.sliding(2).map{case Seq(v1, v2) => v1 -> v2}.toSeq.sorted(new PairDistanceOrdering(simLink).reverse)

    var deltaPminusOne = null.asInstanceOf[Double]
    var deltaP = deltaMeasure(currentGrouping)

    currentGrouping = new mutable.HashSet[Orde]
    val dend = new Dendrogram[Vertex, OrderableRangeSet[Vertex, C]](currentGrouping.toArray)
    var remainingPairs = simlinkOrderedvertPairs
    while(remainingPairs.nonEmpty) {
      val (v1, v2) = remainingPairs.head
      deltaPminusOne = deltaP
      deltaP =

      dend.merge(v1, v2, mu(deltaP))
    }
  }

  def run[C <: Ordered[C] : ClassTag](g:Graph, numCats:Int, getOrdered:(Vertex => C)): Set[Set[Vertex]] = {
    val vertices = g.vertices.toArray
    val dend = new Dendrogram(vertices)
    var currentGroups = vertices.zipWithIndex.groupBy{case (v, _) => getOrdered(v)}.mapValues { verts =>
      verts.map(_._2).reduceLeft{case (x,y) => dend.merge(x,y,0.0)}
    }.toSeq.sortBy(_._1).map(_._2).map(i => IndexedVertexSet(dend.children(i), i)) // these groups are adjacent and should stay that way
    val pairHeap = new mutable.PriorityQueue[(IndexedVertexSet, IndexedVertexSet)]()(new SimlinkOrdering(simLink))
    pairHeap ++= currentGroups.sliding(2).map{case Vector(v1, v2) => v1 -> v2}

    var deltaPMinus1:Double = null
    var deltaP = deltaMeasure(currentGroups.map(_.vs))

    while (currentGroups.size > 1) {
      val (idx1, idx2) = pairHeap.dequeue()
      pairHeap.
      currentGroups = currentGroups.filterNot(i => i == idx1 || i == idx2)
      deltaPMinus1 = deltaP
      deltaP = deltaMeasure(currentGroups.map(_.vs) :+ dend.children(idx1) ++ dend.children(idx2))
      currentGroups = currentGroups :+ dend.merge(idx1, idx2, mu(deltaP, deltaPMinus1))
    }

    //var currentGrouping = g.vertices.groupBy(_.attr[C]).mapValues(_.toSet).toSeq.sortBy(_._1).map(_._2)
    /*
    var deltaPMinus1:Double = null
    var deltaP = deltaMeasure(currentGrouping)
    val merges = mutable.ArrayBuffer[(Set[Vertex], Set[Vertex], Double)]()

    for((v1s, v2s, _) <- pairHeap) {
      currentGrouping = currentGrouping.collect {case vs if vs != v1s && vs != v2s => vs} :+ (v1s ++ v2s)
      deltaPMinus1 = deltaP
      deltaP = deltaMeasure(currentGrouping)
      merges.+=((v1s, v2s, mu(deltaP, deltaPMinus1)))
    }
    */
  }
}

trait Edge {
  def to:Vertex
  def from:Vertex
}
trait Vertex extends Attr {
  def edges:Iterable[Edge]
}
trait Graph {

  def vertices:Iterable[Vertex]

}

class Dendrogram[A, SetType <: Set[A]](nodes:Array[SetType]) {

  private val numNodes = (nodes.length * 2) - 1
  private val allNodes = new Array[(SetType, Double)](numNodes)
  private val nodeIdxMap = new mutable.HashMap[SetType, Int]()
  nodes.zipWithIndex foreach {case (n, i) => allNodes(i) = n -> 0.0; nodeIdxMap(n) = i}
  private val merges = new Array[(Int, Int, Double)](nodes.length - 1)
  private var currentIdx = nodes.length

  def merge(n1:SetType, n2:SetType, distance:Double):SetType= {
    val parentId = currentIdx
    currentIdx += 1
    merges(parentId - nodes.length) = (nodeIdxMap(n1), nodeIdxMap(n2), distance)
    val parentSet = (n1 ++ n2).asInstanceOf[SetType]
    nodeIdxMap(parentSet) = parentId
    allNodes(parentId) = parentSet -> distance
    parentSet
  }

  def mergeId(n1:Int, n2:Int, distance:Double):Int = {
    val parentId = currentIdx
    currentIdx += 1
    merges(parentId - nodes.length) = (n1, n2, distance)
    val parentSet = (allNodes(n1)._1 ++ allNodes(n2)._1).asInstanceOf[SetType]
    nodeIdxMap(parentSet) = parentId
    allNodes(parentId) = parentSet -> distance
    parentId
  }

  @inline
  def children(parent:Int):SetType = allNodes(parent)._1

  @inline
  def index(a:SetType):Int = nodeIdxMap(a)

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
