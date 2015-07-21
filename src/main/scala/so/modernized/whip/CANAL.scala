package so.modernized.whip

import cc.factorie.util.Attr
import org.apache.spark.graphx.Graph
import scala.collection.mutable
import scala.reflect.ClassTag


/**
 * Created by johnsullivan on 7/20/15.
 */
/*
trait CANAL {

  def simLink(v1s:Iterable[Vertex], v2s:Iterable[Vertex]):Double
  def deltaMeasure(grouping:Iterable[Set[Vertex]]):Double
  def mu(d:Double, dm1:Double) = (d - dm1)/dm1

  def run[C <: Ordering : ClassTag](g:Graph, numCats:Int): Set[Set[Vertex]] = {
    var currentGrouping = g.vertices.groupBy(_.attr[C]).mapValues(_.toSet).toSeq.sortBy(_._1).map(_._2)
    val pairHeap = currentGrouping.sliding(2).map{case Vector(v1, v2) => (v1, v2, simLink(v1,v2))}.toSeq.sortBy(_._3)
    var deltaPMinus1:Double = null
    var deltaP = deltaMeasure(currentGrouping)
    val merges = mutable.ArrayBuffer[(Set[Vertex], Set[Vertex], Double)]()

    for((v1s, v2s, _) <- pairHeap) {
      currentGrouping = currentGrouping.collect {case vs if vs != v1s && vs != v2s => vs} :+ (v1s ++ v2s)
      deltaPMinus1 = deltaP
      deltaP = deltaMeasure(currentGrouping)
      merges.+=((v1s, v2s, mu(deltaP, deltaPMinus1)))
    }
  }
}
*/



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
/*
sealed trait DendrogramNode[A] {
  def members:Iterable[A]
}
case class DendrogramLeaf[A](a:A) extends DendrogramNode[A] {
  val members = Some(a)
}
case class DendrogramInternalNode[A] extends DendrogramNode[A] {

}
*/

class Dendrogram[A](private val nodes:Array[A]) {

  private val numNodes = (nodes.length * 2) - 1
  private val merges = new Array[(Int, Int, Double)](nodes.length - 1)
  private var currentIdx = nodes.length

  def merge(n1:Int, n2:Int, distance:Double):Int = {
    val parentId = currentIdx
    currentIdx += 1
    merges(parentId - nodes.length) = (n1, n2, distance)
    parentId
  }

  def links:Iterable[(Int, Int, Double)] = merges

  def children(parent:Int):Set[A] = if(parent < nodes.length) {
    Set(nodes(parent))
  } else {
    val (c1, c2, _) = merges(parent - nodes.length)
    children(c1) ++ children(c2)
  }

  def groupings(numGroups:Int = 1):Iterable[Set[A]] = {
    val usableLinks = merges.sortBy(-_._3).drop(numGroups - 1).toSet
    val clusters = new Array[Set[A]](numNodes) // immutable set for structure sharing in the buildup
    nodes.indices foreach {idx => clusters(idx) = Set(nodes(idx))}
    (nodes.length until numNodes) foreach {idx => clusters(idx) = Set.empty[A]}
    @inline
    def updateChild(child:Int, parent:Int): Unit = {
      clusters(parent) = clusters(parent) ++ clusters(child)
      clusters(child) = Set.empty[A]
    }

    merges.zipWithIndex.collect { case (t@ (child1, child2, _), idx) if usableLinks.contains(t) => // we only take cluster paths that
      val parent = idx + nodes.length
        updateChild(child1, parent)
        updateChild(child2, parent)
    }
    clusters.filter(_.nonEmpty)
  }

  def toDotString:String = {
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
        sb append leafString.format(idx, elem.toString)
    }
    sb.append("}")
    sb.toString()
  }
}

object Dendrogram {
  def main(args:Array[String]): Unit = {
    val dend = new Dendrogram(Array("a", "b", "c", "d", "e", "f"))

    dend.merge(0,1,0.1)
    dend.merge(2,3,0.3)
    dend.merge(7,4,0.6)
    dend.merge(6,5,0.7)
    dend.merge(9,8,0.8)

    //println(dend.toDotString)
    println(dend.groupings(3))
    //println(dend.children(8))
  }
}
