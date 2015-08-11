package so.modernized.whip

import org.openanzo.glitter.query.PatternSolution
import org.openanzo.rdf.Value


package object sparql {


  class PatternSolutionIterator private[sparql] (private val ps:PatternSolution) extends Iterator[Value] {
    private var idx = 0
    override def hasNext: Boolean = idx < ps.size()

    override def next(): Value = {
      val res = ps.getValue(idx)
      idx += 1
      res
    }

    def next[A](implicit conv:PSConvert[A]):A = conv.convert(next())
    def apply[A:PSConvert] = next[A]
  }

  implicit class PatternSolutionExtras(val ps:PatternSolution) extends AnyVal {
    def single[A](implicit conv:PSConvert[A]) = conv.convert(ps.getValue(0))
    def extract[A](extractor:(PatternSolutionIterator => A)) = extractor(new PatternSolutionIterator(ps))
  }

}


