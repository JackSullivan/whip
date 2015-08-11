package so.modernized.whip.sparql

import javax.xml.datatype.XMLGregorianCalendar
import org.joda.time.DateTime
import org.openanzo.rdf.{Value, URI, PlainLiteral, TypedLiteral}
import org.openanzo.rdf.vocabulary.XMLSchema

trait PSConvert[A] {
  def convert(v:Value):A
  final def apply(i:PatternSolutionIterator):A = convert(i.next())
}

object PSConvert {
  implicit object V2URI extends PSConvert[URI] {
    def convert(v:Value) = v match { case uri:URI => uri}
  }
  implicit object V2String extends PSConvert[String] {

    def convert(v:Value) = v match {
      case pl:PlainLiteral => pl.getNativeValue.asInstanceOf[String]
      case tl:TypedLiteral if tl.getDatatypeURI == XMLSchema.STRING => tl.getNativeValue.asInstanceOf[String]
    }
  }

  implicit object V2Int extends PSConvert[Int] {
    def convert(v:Value) = v match {
      case lit:TypedLiteral if lit.getDatatypeURI == XMLSchema.INT =>
        lit.getNativeValue.asInstanceOf[java.lang.Integer].toInt
    }
  }

  implicit object V2Double extends PSConvert[Double] {
    def convert(v:Value) = v match {
      case lit:TypedLiteral if lit.getDatatypeURI == XMLSchema.DOUBLE =>
        lit.getNativeValue.asInstanceOf[java.lang.Double].toDouble
    }
  }

  implicit object V2Boolean extends PSConvert[Boolean] {
    def convert(v:Value) = v match {
      case lit:TypedLiteral if lit.getDatatypeURI == XMLSchema.BOOLEAN =>
        lit.getNativeValue.asInstanceOf[java.lang.Boolean].booleanValue()
    }
  }

  implicit object V2Date extends PSConvert[DateTime] {
    def convert(v:Value) = v match {
      case lit:TypedLiteral if lit.getDatatypeURI == XMLSchema.DATE || lit.getDatatypeURI == XMLSchema.DATETIME =>
        new DateTime(lit.getNativeValue.asInstanceOf[XMLGregorianCalendar].toGregorianCalendar)
    }
  }

}

