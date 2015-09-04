package so.modernized.whip

import com.cambridgesemantics.anzo.unstructured.graphsummarization.XMLUnapplicable._
import com.cambridgesemantics.anzo.utilityservices.common.EncodingUtils
import edu.umd.cs.psl.database.ReadOnlyDatabase
import edu.umd.cs.psl.model.argument.GroundTerm
import edu.umd.cs.psl.model.predicate.SpecialPredicate
import org.openanzo.client.IAnzoClient
import org.openanzo.rdf.{Statement, URI => AnzoURI}
import so.modernized.whip.URIUniqueId.PSLURI
import so.modernized.whip.sparql.QueryIterator
import so.modernized.whip.sparql._
import so.modernized.whip.util.Semigroup._
import scala.collection.JavaConverters._


trait SparqlPredicate extends SpecialPredicate {
  var isComputed = false
  val threshold = 0.5
  val batchSize = 100
  val namedGraph = EncodingUtils.uri("http://TempPslFunctions")
  val predicate = EncodingUtils.uri("http://TempPsl" + this.getName)
  val valuePredicate = EncodingUtils.uri("http://TempPslValue" + this.getName)
  def domain:AnzoURI
  def range:AnzoURI
  def anzo:IAnzoClient
  def dataSets:Set[AnzoURI]
  def computeUnderlying(db:ReadOnlyDatabase, args:GroundTerm*):Double

  final def computeValue(db:ReadOnlyDatabase, args:GroundTerm*):Double = {
    if(isComputed) {
      val Seq(PSLURI(s), PSLURI(o)) = args
      anzo.serverQuery(null, null, dataSets.asJava, s"SELECT ?v WHERE { <${combine(s,o)}> <$valuePredicate> ?v }")
        .getSelectResults.asScala.headOption.map(_.single[Double]).getOrElse(0.0)
    } else {
      computeUnderlying(db, args:_*)
    }
  }
  def precompute(db:PSLSparqlDatabase): Unit = {

    val mkQuery = {(cursor:Int, bSize:Int) =>
      """SELECT ?s1 ?s2
        |WHERE {
        | ?s1 a %s .
        | ?s2 a %s .
        | FILTER(STR(?s1) > STR(?s2)) .
        |}
        |ORDER BY ?s1
        |OFFSET %d
        |LIMIT %d
      """.stripMargin.format(domain, range, cursor, bSize)
    }
    new QueryIterator(anzo, dataSets)(mkQuery, batchSize) foreach { pss =>
      val toCreate = pss.par.flatMap{ ps =>
        val m = ps.toMap
        val XMLURI(s1) = m("s1")
        val XMLURI(s2) = m("s2")
        val res = this.computeUnderlying(new ReadOnlyDatabase(db), xml2Psl(s1), xml2Psl(s2))
        Seq(new Statement(combine(s1,s2), valuePredicate, xmlWrap(res), namedGraph)) ++ (if(this.computeUnderlying(new ReadOnlyDatabase(db), xml2Psl(s1), xml2Psl(s2)) > threshold) {
          Some(new Statement(s1, predicate, s2, namedGraph))
        } else {
          None
        }).toSeq
      }.seq
      anzo add toCreate.asJava
      anzo.commit()
      anzo.updateRepository(true)
    }
    isComputed = true
  }
}

