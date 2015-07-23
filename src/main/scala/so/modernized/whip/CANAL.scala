package so.modernized.whip

import cc.factorie.util.Hooks3

import scala.collection.mutable
import scala.io.Source

class IndexedVertexSeq(val value:Seq[Vertex], val idx:Int) extends (Seq[Vertex], Int)(value, idx) {
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

  private val participationMemo = mutable.AnyRefMap.empty[(IndexedVertexSeq, IndexedVertexSeq), mutable.AnyRefMap[EdgeType, Int]].withDefault(_ => mutable.AnyRefMap.empty)
  private val partMemoRef = mutable.AnyRefMap.empty[IndexedVertexSeq, mutable.HashSet[(IndexedVertexSeq, IndexedVertexSeq)]].withDefault(_ => mutable.HashSet.empty)

  private def participation(xs:IndexedVertexSeq, ys:IndexedVertexSeq, tpy:EdgeType) = {
    val args = xs -> ys
    partMemoRef(xs) = partMemoRef(xs) += args
    partMemoRef(ys) = partMemoRef(ys) += args

    participationMemo(args).getOrElseUpdate(tpy, xs.valueSet.count { x =>
      x.edges(tpy).exists{ e =>
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

  private def participationRatio(xs:IndexedVertexSeq, ys:IndexedVertexSeq, eType:EdgeType) = (participation(xs, ys, eType) + participation(ys, xs, eType)).toDouble / (xs.value.size + ys.value.size)

  // this calls participation many times, but participation's result is memoized
  def smallDelta(xs:IndexedVertexSeq, ys:IndexedVertexSeq, eType:EdgeType) = if(participationRatio(xs, ys, eType) <= 0.5) {
    participation(xs, ys, eType)
  } else {
    ys.value.size - participation(xs, ys, eType)
  }

  def deltaMeasure(grouping:Seq[IndexedVertexSeq], edgeTypes:Iterable[EdgeType]) =
    edgeTypes.flatMap { eType =>
      grouping.flatMap { xs =>
        grouping.map { ys =>
          smallDelta(xs, ys, eType)
        }
      }
    }.sum

  def simLink(i:IndexedVertexSeq, j:IndexedVertexSeq, grouping:Seq[IndexedVertexSeq], edgeTypes:Iterable[EdgeType]) = {
    val res = edgeTypes.flatMap{ eType =>
      grouping.collect { case k if k != i && k != j =>
        math.abs(participationRatio(i,k, eType) - participationRatio(j, k, eType))
      }
    }.sum
    //println("simLink between %d and %d is %.4f".format(i.idx, j.idx, res))
    res
  }
}

trait CANAL {

  // when implemented, this should be memoized for speed
  def simLink(v1s:IndexedVertexSeq, v2s:IndexedVertexSeq, grouping:Seq[IndexedVertexSeq], edgeTypes:Iterable[EdgeType]):Double
  def deltaMeasure(grouping:Seq[IndexedVertexSeq], edgeTypes:Iterable[EdgeType]):Int
  def mu(d:Int, dm1:Int) = (d - dm1).toDouble/dm1

  val mergeHooks = new Hooks3[IndexedVertexSeq, IndexedVertexSeq, IndexedVertexSeq]

  def run[C](g:Graph, numCats:Int, getOrdered:(Vertex => C))(implicit toOrdered:Ordering[C]) = {
    println("running CANAL")
    def updateSimlinkPairs(grouping:Seq[IndexedVertexSeq]) =
      grouping.sliding(2).collect{ case Seq(pair1, pair2) => pair1 -> pair2 }.toSeq.sortBy{ case (v1, v2) => simLink(v1, v2, grouping, g.edgeTypes)}

      //.toSeq.sortBy { case (v1, v2) => simLink(v1, v2, grouping, g.edgeTypes) }

    var currentGrouping = g.vertices.groupBy(getOrdered).toSeq.sortBy(_._1).zipWithIndex.map { case ((_, vs),idx) => new IndexedVertexSeq(vs.toSeq, idx) }
    println("found %d initial groups".format(currentGrouping.size))

    var remainingPairs = updateSimlinkPairs(currentGrouping)
    println("calculated simlinks, for " + remainingPairs.size + " pairs")

    var deltaPminusOne = null.asInstanceOf[Int]
    var deltaP = deltaMeasure(currentGrouping, g.edgeTypes)
    println("deltaP:" + deltaP)

    val numLeaves = currentGrouping.length
    var currentIdx = numLeaves
    val merges = new Array[(Int, Int, Double)](numLeaves -1)
    val mergeHeap = new mutable.PriorityQueue[(Int, Int, Double)]()(Ordering.by({tup:(Int, Int, Double) => tup._3}))
    val subclusters = new Array[IndexedVertexSeq]((numLeaves * 2) - 1)
    currentGrouping foreach {iv => subclusters(iv.idx) = iv}
    println("entering while")

    while(remainingPairs.nonEmpty) {
      val (m1, m2) = remainingPairs.head
      println("merging cluster %d of size %d and cluster %d of size %d".format(m1.idx, m1.value.size, m2.idx, m2.value.size))
      println("before reshuffle had %d groupings".format(currentGrouping.size))
      currentGrouping = currentGrouping.sliding(2).collect {
        case Seq(v1, v2) if v1 != m1 && v1 != m2 => v1
        case Seq(v1, v2) if v1 == m1 =>
          val parentIdx = currentIdx
          currentIdx += 1
          val newCluster = m1 ++ (m2, parentIdx)
          mergeHooks(m1, m2, newCluster)
          subclusters(parentIdx) = newCluster
          newCluster
      }.toSeq
      println("after reshuffle had %d groupings".format(currentGrouping.size))

      println("before recalc had %d remaining pairs".format(remainingPairs.size))
      remainingPairs = updateSimlinkPairs(currentGrouping)
      println("after recalc had %d remaining pairs".format(remainingPairs.size))

      deltaPminusOne = deltaP
      deltaP = deltaMeasure(currentGrouping, g.edgeTypes)
      println("deltaP: %d deltaP - 1: %d".format(deltaP, deltaPminusOne))

      mergeHeap.enqueue((m1.idx, m2.idx, mu(deltaP, deltaPminusOne)))
    }

    mergeHeap foreach println
    val cutoffs = (0 until numCats).map { _ =>
      val (v1, _, _) = mergeHeap.dequeue()
      val cutoff = getOrdered(subclusters(v1).value.last)
      println("found cutoff at" + cutoff)
      cutoff
    }
    cutoffs
  }
}

trait Edge {
  def to:Vertex
  def from:Vertex
}
trait EdgeType
trait Vertex {
  def edges:Map[EdgeType, Iterable[Edge]]
}
trait Graph {
  def edgeTypes:Iterable[EdgeType]
  def vertices:Iterable[Vertex]
}

case class Flight(id:Int, arrivalDelay:Int, toS:String, fromS:String, planeS:String) {
  Flight.toFlights(toS) = Flight.toFlights(toS).+=(this)
  Flight.fromFlights(fromS) = Flight.fromFlights(fromS).+=(this)
  Flight.planeFlights(planeS) = Flight.planeFlights(planeS).+=(this)

  lazy val to = Flight.toFlights(toS).toSet - this
  lazy val from = Flight.fromFlights(fromS).toSet - this
  lazy val plane = Flight.planeFlights(planeS).toSet - this
}

object Flight {
  val toFlights = new mutable.HashMap[String, mutable.ArrayBuffer[Flight]].withDefault(_ => mutable.ArrayBuffer.empty[Flight])
  val fromFlights = new mutable.HashMap[String, mutable.ArrayBuffer[Flight]].withDefault(_ => mutable.ArrayBuffer.empty[Flight])
  val planeFlights = new mutable.HashMap[String, mutable.ArrayBuffer[Flight]].withDefault(_ => mutable.ArrayBuffer.empty[Flight])

  def fromCsvString(str:String) = {
    val Array(id, arrDel, toS, fromS, planeS) = str.split(",")
    val arrDelNum = if(arrDel == "NA") 0 else arrDel.toInt
    Flight(id.toInt, arrDelNum, toS, fromS, planeS)
  }
}

case object SharesTo extends EdgeType
case object SharesFrom extends EdgeType
case object SharesPlane extends EdgeType
case class FlightEdge(to:FlightVertex, from:FlightVertex) extends Edge
class FlightVertex(val f:Flight) extends Vertex{
  val _edges = mutable.HashMap.empty[EdgeType, Iterable[Edge]]
  def edges = _edges.toMap
}

class FlightGraph(flights:Array[Flight]) extends Graph {
  val _vertices = flights.map (new FlightVertex(_))
  println("made verts")

  private var i = 0
  while(i < flights.length) {
    val f = flights(i)
    val vert = _vertices(i)
    vert._edges(SharesTo) = f.to.map(toF => FlightEdge(_vertices(toF.id-1), vert))
    vert._edges(SharesFrom) = f.from.map(toF => FlightEdge(_vertices(toF.id-1), vert))
    vert._edges(SharesPlane) = f.plane.map(toF => FlightEdge(_vertices(toF.id-1), vert))
    i += 1
    if(i % 1000 == 0) {
      print(".")
    }
  }

  println()
  println("Done")

  def vertices = _vertices.toIterable
  val edgeTypes = Seq(SharesTo, SharesFrom, SharesPlane)
}

object CANALImpl extends CANAL with StandardMemoizedSimlink

object CanalTest {
  def main(args:Array[String]): Unit = {
    val flightGraph = new FlightGraph(Source.fromFile(args(0)).getLines().map(Flight.fromCsvString).toArray)
    println("loaded graph")

    println(CANALImpl.run(flightGraph, 3, {v:Vertex => v.asInstanceOf[FlightVertex].f.arrivalDelay}))
  }
}
