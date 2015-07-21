package so.modernized

/**
 * Created by johnsullivan on 7/2/15.
 */
package object whip {

  object DoubleS{
    def unapply(s:String):Option[Double] = if(s.nonEmpty) {Some(s.toDouble)} else {None}
  }

  object IntS{
    def unapply(s:String):Option[Int] = if(s.nonEmpty) {Some(s.toInt)} else {None}
  }

}

