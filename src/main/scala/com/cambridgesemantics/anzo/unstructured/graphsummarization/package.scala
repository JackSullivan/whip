package com.cambridgesemantics.anzo.unstructured

import com.cambridgesemantics.anzo.utilityservices.common.EncodingUtils
import org.openanzo.glitter.query.PatternSolution
import org.openanzo.rdf.{MemVariable, Value}

import scala.annotation.tailrec

package object graphsummarization {

  
  implicit class MapExtras[K,V1](m1:Map[K,V1]) {
    def |+|[V,V2](m2:Map[K,V2])(implicit combiner:(Option[V1], Option[V2]) => V):Map[K,V] = {
      (m1.keySet ++ m2.keySet).map { k =>
        k -> combiner(m1.get(k), m2.get(k))
      }.toMap
    }
  }

  implicit class UriStringContext(val sc:StringContext) extends AnyVal {
    def uri(args:Any*) = EncodingUtils.uri(sc.parts.mkString)
  }

  implicit class PatternSolutionExtras(val ps:PatternSolution) extends AnyVal {
    def single[A](implicit conv:XMLUnapplicable[A]) = conv.unapply(ps.getValue(0)).get
    def values:List[Value] = (0 until ps.size()).map(ps.getValue).toList
    def toMap = (0 until ps.size()).map { idx =>
      ps.getBinding(idx).asInstanceOf[MemVariable].getName -> ps.getValue(idx)
    }.toMap
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
