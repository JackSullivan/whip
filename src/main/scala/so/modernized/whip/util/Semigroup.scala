package so.modernized.whip.util

import org.openanzo.rdf.URI
import org.openanzo.rdf.utils.UriGenerator

trait Semigroup[A] {
  def combine(a1:A, a2:A):A
}

object Semigroup {
  implicit object URISemigroup extends Semigroup[URI] {
    def combine(u1:URI, u2:URI) = UriGenerator.generateLdsCatalogEntryUri(u1, u2)
  }

  def combine[A](a1:A, a2:A)(implicit ev:Semigroup[A]) = ev.combine(a1, a2)
}
