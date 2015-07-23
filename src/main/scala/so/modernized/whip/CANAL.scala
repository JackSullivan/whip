package so.modernized.whip

import cc.factorie.util.Hooks3

import scala.collection.mutable
import scala.reflect.ClassTag


class IndexedVertexSeq(val value:Seq[Vertex], val idx:Int) extends (Seq[Vertex], Int) {
  override def hashCode = idx
  override def equals(obj:Any) = obj match {
    case ref:AnyRef if this eq ref => true
    case that:IndexedVertexSeq => this.idx == that.idx
    case otw => false
  }
  lazy val valueSet = value.toSet

  def ++(other:IndexedVertexSeq, newIdx:Int) = new IndexedVertexSeq(this.value ++ other.value, newIdx)
}

trait StandardMemoizedSimlink {
  this: CANAL =>

  private val participationMemo = mutable.HashMap.empty[(IndexedVertexSeq, IndexedVertexSeq), Int]
  private val partMemoRef = mutable.HashMap.empty[IndexedVertexSeq, mutable.HashSet[(IndexedVertexSeq, IndexedVertexSeq)]].withDefault(_ => mutable.HashSet.empty[(IndexedVertexSeq, IndexedVertexSeq)])

  private def participation(xs:IndexedVertexSeq, ys:IndexedVertexSeq) = {
    partMemoRef(xs) = partMemoRef(xs) += xs -> ys
    partMemoRef(ys) = partMemoRef(ys) += xs -> ys
    participationMemo.getOrElseUpdate(xs -> ys,xs.valueSet.count { x =>
      x.edges.exists{ e =>
        ys.valueSet.contains(e.to)
      }
    })
  }
  
  // this method removes memoized values that will no longer be used to keep them from growing too large
  mergeHooks += { case (m1, m2, _) =>
      partMemoRef(m1) foreach participationMemo.remove
      partMemoRef remove m1
      partMemoRef(m2) foreach participationMemo.remove
      partMemoRef remove m2
  }

  private def participationRatio(xs:IndexedVertexSeq, ys:IndexedVertexSeq) = (participation(xs, ys) + participation(ys, xs)).toDouble / (xs.value.size + ys.value.size)

  // this calls participation many times, but participation's result is memoized
  def smallDelta(xs:IndexedVertexSeq, ys:IndexedVertexSeq) = if(participationRatio(xs, ys) <= 0.5) {
    participation(xs, ys)
  } else {
    ys.value.size - participation(xs, ys)
  }

  def deltaMeasure(grouping:Seq[IndexedVertexSeq]) = grouping.flatMap { xs =>
    grouping.map { ys =>
      smallDelta(xs, ys)
    }
  }.sum

  def simLink(i:IndexedVertexSeq, j:IndexedVertexSeq, grouping:Seq[IndexedVertexSeq]) = {
    grouping.collect { case k if k != i && k != j =>
      math.abs(participationRatio(i,k) - participationRatio(j, k))
    }.sum
  }
}

trait CANAL {

  // when implemented, this should be memoized for speed
  def simLink(v1s:IndexedVertexSeq, v2s:IndexedVertexSeq, grouping:Seq[IndexedVertexSeq]):Double
  def deltaMeasure(grouping:Seq[IndexedVertexSeq]):Int
  def mu(d:Int, dm1:Int) = (d - dm1).toDouble/dm1

  val mergeHooks = new Hooks3[IndexedVertexSeq, IndexedVertexSeq, IndexedVertexSeq]

  def run[C <: Ordered[C] : ClassTag](g:Graph, numCats:Int, getOrdered:(Vertex => C))(implicit toOrdered:C => Ordered[C]) = {
    def updateSimlinkPairs(grouping:Seq[IndexedVertexSeq]) = grouping.sliding(2).collect{ case Seq(pair1, pair2) => pair1 -> pair2 }.toSeq.sortBy { case (v1, v2) => simLink(v1, v2, grouping) }

    //var currentGrouping = g.vertices.groupBy(getOrdered).values.toSeq.map(_.toSeq).sorted(new RangeSeqOrdering(getOrdered)).zipWithIndex
    var currentGrouping = g.vertices.groupBy(getOrdered).toSeq.sortBy(_._1).zipWithIndex.map { case ((_, vs),idx) => new IndexedVertexSeq(vs.toSeq, idx) }

    var remainingPairs = updateSimlinkPairs(currentGrouping)

    var deltaPminusOne = null.asInstanceOf[Int]
    var deltaP = deltaMeasure(currentGrouping)

    val numLeaves = currentGrouping.length
    var currentIdx = numLeaves
    val merges = new Array[(Int, Int, Double)](numLeaves -1)
    val mergeHeap = new mutable.PriorityQueue[(Int, Int, Double)]()(Ordering.by({tup:(Int, Int, Double) => tup._3}))
    val subclusters = new Array[IndexedVertexSeq]((numLeaves * 2) - 1)
    currentGrouping foreach {case (v, i) => subclusters(i) = v}

    while(remainingPairs.nonEmpty) {
      val (m1, m2) = remainingPairs.head
      currentGrouping = currentGrouping.sliding(2).collect {
        case Vector(v1, v2) if v1 != m1 && v1 != m2 => v1
        case Vector(v1, v2) if v1 == m1 =>
          val parentIdx = currentIdx
          currentIdx += 1
          val newCluster = m1 ++ (m2, parentIdx)
          mergeHooks(m1, m2, newCluster)
          subclusters(parentIdx) = newCluster
          newCluster
      }.toSeq
      remainingPairs = updateSimlinkPairs(currentGrouping)

      deltaPminusOne = deltaP
      deltaP = deltaMeasure(currentGrouping)

      mergeHeap.enqueue((m1.idx, m2.idx, mu(deltaP, deltaPminusOne)))
    }

    val cutoffs = (0 until numCats).map { _ =>
      val (v1, _, _) = mergeHeap.dequeue()
      getOrdered(subclusters(v1).value.last)
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
