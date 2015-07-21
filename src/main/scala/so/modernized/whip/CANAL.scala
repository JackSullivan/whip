package so.modernized.whip

import cc.factorie.app.nlp.hcoref.{Mention, NodeVariables}
import cc.factorie.util.Attr
import cc.factorie.variable.{SetVariable, Var, DiffList}
import org.apache.spark.graphx.Graph
import scala.StringBuilder
import scala.collection.mutable
import scala.reflect.ClassTag


/**
 * Created by johnsullivan on 7/20/15.
 */
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


/*
class Dendrogram[NodeType](leaves:Iterable[NodeType]) {

  class DendrogramVariables(val nodes:SetVariable[NodeType]) extends NodeVariables[DendrogramVariables]{
    def this(node:NodeType) = this{val s = new SetVariable[NodeType]; s.add(node)(null); s}

    override def getVariables: Seq[Var] = Seq(nodes)

    override def ++(other: DendrogramVariables)(implicit d: DiffList): DendrogramVariables = new DendrogramVariables(this.nodes ++ other.nodes)
    override def --(other: DendrogramVariables)(implicit d: DiffList): DendrogramVariables = new DendrogramVariables(this.nodes -- other.nodes)

    override def ++=(other: DendrogramVariables)(implicit d: DiffList) {this.nodes addAll other.nodes.value}
    override def --=(other: DendrogramVariables)(implicit d: DiffList) {this.nodes removeAll other.nodes.value}
  }

  val mentMap = leaves.map(l => l -> new Mention(new DendrogramVariables(l))).toMap

}
*/

/*
class Dendrogram[NodeType](leaves:Iterable[NodeType]) {
  private case class NodeContainer(node:Option[NodeType], joinScore:Double, parentIndex:Int) {
    def updateParent(newParentIndex:Int) = NodeContainer(node, joinScore, newParentIndex)
  }

  // we should store for each node its value (NodeType) its merge score (0.0 for leaves), and its parent (-1 for the root)
  private val nodes = new Array[(Set[NodeType], Double, Int)]((leaves.size*2)-2)
  private val nodeMap = leaves.zipWithIndex.map { case (nt, idx) => nodes(idx) = (Set(nt), 0.0, -1); nt -> idx}.toMap
  private var nextId = leaves.size
  private def getId:Int = {
    val res = nextId
    nextId += 1
    res
  }

  def merge(n1:NodeType, n2:NodeType, score:Double): Unit = {
    val parentId = getId
    NodeContainer(None, score, -1)
    val n1Idx = nodeMap(n1)
    val n2Idx = nodeMap(n2)

  }


}
*/
