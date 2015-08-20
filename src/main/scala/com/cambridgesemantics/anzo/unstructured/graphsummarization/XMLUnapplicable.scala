package com.cambridgesemantics.anzo.unstructured.graphsummarization

import com.cambridgesemantics.anzo.datasource.bigdata.BDTypedLiteral
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
      case tl:BDTypedLiteral if tl.getDatatypeURI == XMLSchema.INT         => Some(tl.intValue())
      case tl:MemTypedLiteral if tl.getDatatypeURI == XMLSchema.INT        => Some(tl.intValue())
      case tl:StoredTypedLiteral if tl.getDatatypeURI == XMLSchema.INT     => Some(tl.intValue())
      case tl:BDTypedLiteral     if tl.getDatatypeURI == XMLSchema.INTEGER => Some(tl.integerValue().intValue())
      case tl:MemTypedLiteral if tl.getDatatypeURI == XMLSchema.INTEGER    => Some(tl.integerValue().intValue())
      case tl:StoredTypedLiteral if tl.getDatatypeURI == XMLSchema.INTEGER => Some(tl.integerValue().intValue())
      case _ => None
    }
  }

  implicit object XMLDouble extends XMLUnapplicable[Double] {
    def unapply(v:Value) = v match {
      case tl:BDTypedLiteral if tl.getDatatypeURI == XMLSchema.DOUBLE      => Some(tl.doubleValue())
      case tl:MemTypedLiteral if tl.getDatatypeURI == XMLSchema.DOUBLE     => Some(tl.doubleValue())
      case tl:StoredTypedLiteral if tl.getDatatypeURI == XMLSchema.DOUBLE  => Some(tl.doubleValue())
      case tl:BDTypedLiteral if tl.getDatatypeURI == XMLSchema.DECIMAL     => Some(tl.decimalValue().doubleValue())
      case tl:MemTypedLiteral if tl.getDatatypeURI == XMLSchema.DECIMAL    => Some(tl.decimalValue().doubleValue())
      case tl:StoredTypedLiteral if tl.getDatatypeURI == XMLSchema.DECIMAL => Some(tl.decimalValue().doubleValue())
      case _ => None
    }
  }

  implicit object XMLString extends XMLUnapplicable[String] {
    def unapply(v:Value) = v match {
      case pl:PlainLiteral => Some(pl.getNativeValue.asInstanceOf[String])
      case tl:BDTypedLiteral if tl.getDatatypeURI == XMLSchema.STRING     => Some(tl.stringValue())
      case tl:MemTypedLiteral if tl.getDatatypeURI == XMLSchema.STRING    => Some(tl.stringValue())
      case tl:StoredTypedLiteral if tl.getDatatypeURI == XMLSchema.STRING => Some(tl.stringValue())
      case _ => None
    }
  }

  implicit object XMLBoolean extends XMLUnapplicable[Boolean] {
    def unapply(v:Value) = v match {
      case tl:BDTypedLiteral if tl.getDatatypeURI == XMLSchema.BOOLEAN     => Some(tl.booleanValue().booleanValue())
      case tl:MemTypedLiteral if tl.getDatatypeURI == XMLSchema.BOOLEAN    => Some(tl.booleanValue().booleanValue())
      case tl:StoredTypedLiteral if tl.getDatatypeURI == XMLSchema.BOOLEAN => Some(tl.booleanValue().booleanValue())
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
      case tl:BDTypedLiteral if tl.getDatatypeURI == XMLSchema.FLOAT     => Some(tl.floatValue())
      case tl:MemTypedLiteral if tl.getDatatypeURI == XMLSchema.FLOAT    => Some(tl.floatValue())
      case tl:StoredTypedLiteral if tl.getDatatypeURI == XMLSchema.FLOAT => Some(tl.floatValue())
      case _ => None
    }
  }

  implicit object XMLLong extends XMLUnapplicable[Long] {
    def unapply(v:Value) = v match {
      case tl:BDTypedLiteral if tl.getDatatypeURI == XMLSchema.LONG     => Some(tl.longValue())
      case tl:MemTypedLiteral if tl.getDatatypeURI == XMLSchema.LONG    => Some(tl.longValue())
      case tl:StoredTypedLiteral if tl.getDatatypeURI == XMLSchema.LONG => Some(tl.longValue())
      case _ => None
    }
  }

  implicit object XMLDate extends XMLUnapplicable[DateTime] {
    def unapply(v:Value) = v match {
      case tl:BDTypedLiteral if tl.getDatatypeURI == XMLSchema.DATE         => Some(new DateTime(tl.calendarValue()))
      case tl:MemTypedLiteral if tl.getDatatypeURI == XMLSchema.DATE        => Some(new DateTime(tl.calendarValue()))
      case tl:StoredTypedLiteral if tl.getDatatypeURI == XMLSchema.DATE     => Some(new DateTime(tl.calendarValue()))
      case tl:BDTypedLiteral if tl.getDatatypeURI == XMLSchema.TIME         => Some(new DateTime(tl.calendarValue()))
      case tl:MemTypedLiteral if tl.getDatatypeURI == XMLSchema.TIME        => Some(new DateTime(tl.calendarValue()))
      case tl:StoredTypedLiteral if tl.getDatatypeURI == XMLSchema.TIME     => Some(new DateTime(tl.calendarValue()))
      case tl:BDTypedLiteral if tl.getDatatypeURI == XMLSchema.DATETIME     => Some(new DateTime(tl.calendarValue()))
      case tl:MemTypedLiteral if tl.getDatatypeURI == XMLSchema.DATETIME    => Some(new DateTime(tl.calendarValue()))
      case tl:StoredTypedLiteral if tl.getDatatypeURI == XMLSchema.DATETIME => Some(new DateTime(tl.calendarValue()))
      case _ => None
    }
  }
}


