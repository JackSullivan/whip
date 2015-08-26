package so.modernized.whip

import scala.annotation.tailrec

package object util {
  implicit class MapExtras[K,V1](m1:Map[K,V1]) {
    def |+|[V,V2](m2:Map[K,V2])(implicit combiner:(Option[V1], Option[V2]) => V):Map[K,V] = {
      (m1.keySet ++ m2.keySet).map { k =>
        k -> combiner(m1.get(k), m2.get(k))
      }.toMap
    }
  }

  def pairs[A](as:Iterable[A]):List[(A, A)] = {
    @tailrec
    def pairsHelper(build:List[(A, A)], xs:List[A]):List[(A, A)] = xs match {
      case x :: rest => pairsHelper(build ::: rest.map(x -> _), rest)
      case Nil => build
    }
    pairsHelper(List.empty[(A, A)], as.toList)
  }

}
