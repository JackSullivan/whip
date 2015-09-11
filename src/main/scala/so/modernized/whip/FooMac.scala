package so.modernized.whip

import scala.reflect.macros.whitebox
import scala.language.experimental.macros

sealed trait FooState
case object Unset extends FooState
case class Set(state:String) extends FooState



object Foo {
  var fooState: FooState = Unset

  def fmStartFoo(state: String): Unit = {
    fooState = Set(state)
  }

  def fmInFoo(fmDo: String) = {
    fooState match {
      case Set(state) =>
        println("doing: " + fmDo + " with state: " + state)
      case _ => throw new IllegalArgumentException("foo needs to be set, was " + fooState)
    }
  }

  def fmFinishFoo(): Unit = {
    fooState = Unset
  }

}


object FooMac {
  def withFooImpl(c:whitebox.Context)(state:c.Expr[String])(block:c.Tree):c.Tree = {
    import c.universe._
    val foo = symbolOf[Foo.type].asClass.module
    val tree = q"$foo.fmStartFoo($state); $block; $foo.fmFinishFoo()"
    println(tree)
    tree
  }
  def withFoo(state:String)(block: => Unit):Unit = macro FooMac.withFooImpl
}
