package com.cambridgesemantics.anzo.unstructured.graphsummarization

import org.joda.time.DateTime
import org.openanzo.rdf.vocabulary.XMLSchema
import org.openanzo.rdf.{URI => AnzoURI, _}


trait XMLUnapplicable[A] { xmlun =>
  def unapply(v:Value):Option[A]
  def fn = new PartialFunction[Value, A] {
    private var lastCheck:Value = null
    private var lastRes:Option[A] = None
    def isDefinedAt(x: Value) = {
      lastCheck = x
      lastRes = xmlun.unapply(lastCheck)
      lastRes.isDefined
    }

    def apply(v1: Value): A = {
      if(!v1.eq(lastCheck)) isDefinedAt(v1)
      lastRes.get
    }
  }
}

object XMLUnapplicable {
  def isNumeric(xmlType:AnzoURI) =
    Set(XMLSchema.INT, XMLSchema.INTEGER, XMLSchema.DOUBLE, XMLSchema.DECIMAL, XMLSchema.FLOAT, XMLSchema.LONG).contains(xmlType.asInstanceOf[MemURI])

  def isLink(fieldType:AnzoURI) = fieldType.getNamespace != XMLSchema.NAMESPACE

  implicit object XMLInt extends XMLUnapplicable[Int] {
    def unapply(v:Value) = v match {
      case tl:TypedLiteral if tl.getDatatypeURI == XMLSchema.INT => Some(tl.getNativeValue.asInstanceOf[java.lang.Integer].toInt)
      case tl:TypedLiteral if tl.getDatatypeURI == XMLSchema.INTEGER => Some(tl.getNativeValue.asInstanceOf[java.lang.Integer].toInt)
      case _ => None
    }
  }

  implicit object XMLDouble extends XMLUnapplicable[Double] {
    def unapply(v:Value) = v match {
      case tl:TypedLiteral if tl.getDatatypeURI == XMLSchema.DOUBLE =>
        Some(tl.getNativeValue.asInstanceOf[java.lang.Double].toDouble)
      case tl:TypedLiteral if tl.getDatatypeURI == XMLSchema.DECIMAL =>
        Some(tl.getNativeValue.asInstanceOf[java.math.BigDecimal].doubleValue())
      case _ => None
    }
  }

  implicit object XMLString extends XMLUnapplicable[String] {
    def unapply(v:Value) = v match {
      case pl:PlainLiteral => Some(pl.getNativeValue.asInstanceOf[String])
      case tl:TypedLiteral if tl.getDatatypeURI == XMLSchema.STRING => Some(tl.getNativeValue.asInstanceOf[String])
      case _ => None
    }
  }

  implicit object XMLBoolean extends XMLUnapplicable[Boolean] {
    def unapply(v:Value) = v match {
      case tl:TypedLiteral if tl.getDatatypeURI == XMLSchema.BOOLEAN =>
        Some(tl.getNativeValue.asInstanceOf[java.lang.Boolean].booleanValue())
      case _ => None
    }
  }

  implicit object XMLURI extends XMLUnapplicable[AnzoURI] {
    def unapply(v:Value) = v match {
      case uri:AnzoURI => Some(uri)
      case _ => None
    }
  }

  implicit object XMLFloat extends XMLUnapplicable[Float] {
    def unapply(v:Value) = v match {
      case tl:TypedLiteral if tl.getDatatypeURI == XMLSchema.FLOAT =>
        Some(tl.getNativeValue.asInstanceOf[java.lang.Float].floatValue())
      case _ => None
    }
  }

  implicit object XMLLong extends XMLUnapplicable[Long] {
    def unapply(v:Value) = v match {
      case tl:TypedLiteral if tl.getDatatypeURI == XMLSchema.LONG =>
        Some(tl.getNativeValue.asInstanceOf[java.lang.Long].longValue())
      case _ => None
    }
  }

  implicit object XMLDate extends XMLUnapplicable[DateTime] {
    def unapply(v:Value) = v match {
      case tl:TypedLiteral if tl.getDatatypeURI == XMLSchema.DATE || tl.getDatatypeURI == XMLSchema.DATETIME =>
        Some(new DateTime(tl.getNativeValue))
      case _ => None
    }
  }
}


