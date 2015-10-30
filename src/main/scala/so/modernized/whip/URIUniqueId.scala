package so.modernized.whip

import edu.umd.cs.psl.model.argument.{UniqueID, GroundTerm, ArgumentType}
import so.modernized.psl_scala.primitives.PSLUnapplicable
import org.openanzo.rdf.{URI => AnzoURI}

object URIUniqueId {
  implicit  object PSLURI extends PSLUnapplicable[AnzoURI] {
    override type T = URIUniqueId

    val argType = ArgumentType.UniqueID
    def unapply(v: GroundTerm) = v match {
      case v:URIUniqueId => Some(v.id)
      case _ => None
    }
    def apply(a: AnzoURI) = new URIUniqueId(a)
  }
}

class URIUniqueId(val id:AnzoURI) extends UniqueID {
  override def getInternalID = id

  override def compareTo(o: GroundTerm) = o match {
    case oUri:URIUniqueId => id compareTo oUri.id
    case oGt:GroundTerm => getClass.getSimpleName compareTo oGt.getClass.getSimpleName
  }

  override def toString = id.toString
  override def hashCode = id.hashCode()
}

