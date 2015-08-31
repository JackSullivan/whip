package so.modernized.whip.util

trait Truthy[A] {
  def toBoolean(a:A):Boolean
  def unapply(a:Any):Option[Boolean]
}

object Truthy {
  implicit object IntTruthy extends Truthy[Int] {
    def toBoolean(i:Int) = i == 1
    def unapply(a:Any) = a match {
      case i:Int => Some(toBoolean(i))
      case _ => None
    }
  }
  implicit object BoolTruthy extends Truthy[Boolean] {
    def toBoolean(b:Boolean) = b
    def unapply(a:Any) = a match {
      case i:Boolean => Some(toBoolean(i))
      case _ => None
    }
  }
  implicit object StringTruthy extends Truthy[String] {
    def toBoolean(s:String) = Set("t", "true", "yes", "y").contains(s.toLowerCase)
    def unapply(a:Any) = a match {
      case i:String => Some(toBoolean(i))
      case _ => None
    }
  }

  def truthy[A](a:A)(implicit ev:Truthy[A]) = ev.toBoolean(a)
}
