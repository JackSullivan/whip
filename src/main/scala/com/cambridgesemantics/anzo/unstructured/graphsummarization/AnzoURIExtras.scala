package com.cambridgesemantics.anzo.unstructured.graphsummarization

import com.cambridgesemantics.anzo.utilityservices.common.EncodingUtils
import org.openanzo.rdf.{URI => AnzoURI}

object AnzoURIExtras {
  implicit val ord = new Ordering[AnzoURI]{
    override def compare(x: AnzoURI, y: AnzoURI): Int = x.toString compare y.toString
  }

  implicit class UriStringContext(val sc:StringContext) extends AnyVal {
    def uri(args:Any*) = EncodingUtils.uri(sc.parts.mkString)
  }

}

