package so.modernized.and.useful

import scala.util.Try

trait Parseable[A, B] extends PartialFunction[A, B] {
  final def apply(a:A):B = unapply(a).get
  final def isDefinedAt(a:A):Boolean = unapply(a).isDefined
  def unapply(a:A):Option[B]
}

object Parseable {
  implicit object IntString extends Parseable[String, Int] {
    def unapply(a: String): Option[Int] = Try(Integer.parseInt(a)).toOption
  }

  implicit object FloatString extends Parseable[String, Float] {
    def unapply(a:String):Option[Float] = Try(java.lang.Float.parseFloat(a)).toOption
  }

  implicit object DoubleString extends Parseable[String, Double] {
    def unapply(a:String):Option[Double] = Try(java.lang.Double.parseDouble(a)).toOption
  }

  def parse[A,B](a:A)(implicit ev:Parseable[A, B]):B = ev(a)
  def getParse[A,B](a:A)(implicit ev:Parseable[A, B]):Option[B] = ev.unapply(a)

  //def twoStep[A, B, C](a:A)(implicit ev1:Parseable[A, B], ev2:Parseable[B, C]):C = ev2(ev1(a))
}

object Test {
  import Parseable._
  def main(args:Array[String]): Unit = {
    val s = "1000"
    val IntString(i) = s
    println(i)

  }
}

