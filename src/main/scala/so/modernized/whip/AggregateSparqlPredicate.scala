package so.modernized.whip

import org.openanzo.glitter.query.PatternSolution
import org.openanzo.rdf.{Statement, URI => AnzoURI}
import so.modernized.whip.sparql.QueryIterator
import scala.collection.JavaConverters._
import util.VectorOps._
import util.Semigroup._
import com.cambridgesemantics.anzo.unstructured.graphsummarization.XMLUnapplicable._

/**
 * This trait is for predicates that can be precomputed by streaming across an aggregated SPARQL function in a single
 * pass, and dotting the output with a weights vectior to produce the final value
 */
trait AggregateSparqlPredicate extends SparqlPredicate {
  def aggregateQuery:String
  def queryPopulators:Seq[String]
  val weights:Array[Double]

  def cutoff(a:PatternSolution, b:PatternSolution):Boolean
  def agg(as:Seq[PatternSolution]):((AnzoURI, AnzoURI), Array[Double])

  override def precompute(db:PSLSparqlDatabase): Unit = {
    println("precomputing " + this.getName)
    val mkQuery = {(cursor:Int, bSize:Int) =>
      aggregateQuery.format(queryPopulators:_*).format(cursor, bSize)
    }

    var idx = 0
    // TODO actually storing all the scores uses *many* more triples than keeping those above the threshold, do we really need them all?
    new AggregatingIterator(new QueryIterator(anzo, dataSets)(mkQuery, batchSize).flatten, agg, cutoff).flatMap {
      case ((s1, s2), feats) =>
        val score = (feats dot weights)/feats.length
        Seq(new Statement(combine(s1, s2), valuePredicate, xmlWrap(score), namedGraph)) ++
          (if (score > threshold) Seq(new Statement(s1, predicate, s2, namedGraph)) else Seq.empty)
    }.grouped(batchSize).foreach { stmts =>
      anzo add stmts.asJava
      anzo.commit()
      anzo.updateRepository(true)
      idx += 1
      println("Loaded %d statements" format (batchSize*idx))
    }
    isComputed = true
  }
}

