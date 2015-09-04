package so.modernized.and.useful

import scala.util.Try

trait Parseable[A, B] extends PartialFunction[A, B] {
  final def unapply(a:A):Option[B] = lift(a)
}

object Parseable {
  implicit object IntString extends Parseable[String, Int] {
    def isDefinedAt(x: String) = Try(Integer.parseInt(x)).isSuccess
    def apply(x: String) = Integer.parseInt(x)
  }

  implicit object FloatString extends Parseable[String, Float] {
    def isDefinedAt(x:String) = Try(java.lang.Float.parseFloat(x)).isSuccess
    def apply(x:String) = java.lang.Float.parseFloat(x)
  }

  implicit object DoubleString extends Parseable[String, Double] {
    def isDefinedAt(x:String) = Try(java.lang.Double.parseDouble(x)).isSuccess
    def apply(x:String) = java.lang.Double.parseDouble(x)
  }

  def parse[A,B](a:A)(implicit ev:Parseable[A, B]):B = ev(a)
  def getParse[A,B](a:A)(implicit ev:Parseable[A, B]):Option[B] = ev.unapply(a)

  //def twoStep[A, B, C](a:A)(implicit ev1:Parseable[A, B], ev2:Parseable[B, C]):C = ev2(ev1(a))
}

object Test {
  import Parseable._
  def main(args:Array[String]): Unit = {
    val s1 = "1000"

    val IntString(i) = s1
    println(i.getClass + "\t" + i)
  }
}
