package so.modernized.whip

import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

class Impl(val c:Context) {
  import c.universe._
  private val newArr = TermName(c.freshName("arr"))
  private val i = TermName(c.freshName("i"))

  def foreachGen(ops:List[c.universe.Tree]):c.Expr[Array[Double]] = {
    val Apply((_, List(n))) = c.prefix.tree
    val all = q"$newArr = new Array[Double]($n.length); $i = 0; while($i < $n.length) { ..$ops; $i += 1 }; $newArr"
    println(all)
    c.Expr[Array[Double]](all)
  }

  def broadcast(op:c.universe.Tree)(v:c.Expr[Array[Double]], s:c.Expr[Double]) = q"$newArr($i) = $v($i) $op $s"
  val scalarPlus = broadcast(q"+")
  val scalarMinus = broadcast(q"-")
  val scalaTimes = broadcast(q"*")
  val scalarDiv = broadcast(q"/")

  def foldlGen(ops:List[c.universe.Tree], accumulate:c.Expr[(Double, Double) => Double]) = {
    val Apply((_, List(n))) = c.prefix.tree
  }
}

object MathMac {

  def foreachGen(c:Context)(ops:List[c.universe.Tree], newArr:c.universe.TermName, i:c.universe.TermName):c.Expr[Array[Double]] = {
    import c.universe._

    c.Expr[Array[Double]](c.prefix.tree match {
      case Apply((_, List(n))) =>

        val all = q"val $newArr = new Array[Double]($n.length); var $i = 0; while($i < $n.length) { ..$ops; $i += 1 }; $newArr"
        println(all)
        all
    })
  }

  //def foldlGen(c:Context)(ops:List[c.universe.Tree], newArr:c.universe.TermName, i:c.universe.TermName):

  def scalarGenImpl(c:Context)(s:c.Expr[Double], v:c.Expr[Array[Double]], op:c.universe.Tree) = {
    import c.universe._

  }

  def scalarPlusImpl(c:Context)(s:c.Expr[Double]) = {
    import c.universe._
    c.prefix.tree match {
      case Apply((_, List(n))) =>
        val newArr = TermName(c.freshName())
        val i = TermName(c.freshName())
        val op = q"$newArr($i) = $n($i) + $s"
        foreachGen(c)(List(op), newArr, i)
    }
  }


  implicit class Vector(v1:Array[Double]) {
    def +(s:Double):Array[Double] = macro scalarPlusImpl
    def -(s:Double):Array[Double] = ???
    def +(v2:Array[Double]):Array[Double] = ???
    def -(v2:Array[Double]):Array[Double] = ???
    def dot(v2:Array[Double]):Double = ???
    def *(s:Double):Array[Double] = ???
    def /(s:Double):Array[Double] = ???
  }

  val v = Array(1.0, 2.0, 3.4)
  v + 3

}
