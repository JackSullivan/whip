package com.cambridgesemantics.anzo.unstructured

import org.openanzo.glitter.query.PatternSolution
import org.openanzo.rdf.{MemVariable, Value}

package object graphsummarization {

  implicit class PatternSolutionExtras(val ps:PatternSolution) extends AnyVal {
    def single[A](implicit conv:XMLUnapplicable[A]) = conv.unapply(ps.getValue(0)).get
    def values:List[Value] = (0 until ps.size()).map(ps.getValue).toList
    def toMap = (0 until ps.size()).map { idx =>
      ps.getBinding(idx).asInstanceOf[MemVariable].getName -> ps.getValue(idx)
    }.toMap
  }

}
