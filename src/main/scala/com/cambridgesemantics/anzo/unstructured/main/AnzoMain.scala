package com.cambridgesemantics.anzo.unstructured.main

import org.joda.time.DateTime
import org.openanzo.client.AnzoClient
import com.cambridgesemantics.anzo.utilityservices.common.EncodingUtils
import org.openanzo.rdf.vocabulary.{XMLSchema, RDF}
import scala.collection.JavaConverters._
import org.openanzo.test.AbstractTest
import org.openanzo.rdf.{MemURI, Value, URI}
import so.modernized.whip.sparql._

case class RdfObject(val dType:URI, val id:URI, val fields:Map[URI, Value]) {
  def |+| (that:RdfObject):RdfObject = {
    require(this.dType == that.dType && this.id == that.id)
    RdfObject(dType, id, this.fields ++ that.fields)
  }
}

object RdfObject{
  def smush(objs:Iterable[RdfObject]) = objs.groupBy { case RdfObject(t,i,_) => t -> i}.values.map(_.reduce(_ |+| _))
}

object AnzoMain {

  object T extends AbstractTest() {
    def conf = getSystemClientConfiguration
  }

  def movie(anzo: AnzoClient) {
    val dataset = EncodingUtils.uri("http://cambridgesemantics.com/datasets/film")

    val query = """PREFIX film: <http://cambridgesemantics.com/ontologies/2009/08/Film#>
                  |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                  |SELECT ?title ?rank
                  |WHERE {
                  |  ?movie rdf:type film:Movie .
                  |  ?movie film:title   ?title .
                  |  ?movie film:ranking ?rank .
                  |}""".stripMargin
    println(query)
    anzo.serverQuery(null, null, Set(dataset).asJava, query).getSelectResults.asScala.take(5) foreach println

  }

  def movieOnt(anzo: AnzoClient) = {
    val dataset = EncodingUtils.uri("http://cambridgesemantics.com/datasets/Film")
    val ont = EncodingUtils.uri("http://cambridgesemantics.com/ontologies/2009/08/Film_frame")
    val reg = EncodingUtils.uri("http://cambridgesemantics.com/registries/Ontologies")
    val query = """PREFIX film: <http://cambridgesemantics.com/ontologies/2009/08/Film#>
                  |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                  |SELECT ?field ?label ?range
                  |WHERE {
                  |  ?field rdfs:domain film:Movie .
                  |  ?field rdfs:range ?range .
                  |  ?field rdfs:label ?label .
                  |}""".stripMargin
    println(query)
    val res = anzo.serverQuery(null, null, Set(reg).asJava, query).getSelectResults
    res.asScala.take(8) foreach println
    res
  }

  type RdfType = URI
  type RdfId = URI
  type RdfFieldDesc = (URI, URI)


  val ontologyService = Set(EncodingUtils.uri("http://cambridgesemantics.com/semanticServices/OntologyService#Frames"))

  def classesFor(ontology:URI)(implicit anzo:AnzoClient):Seq[RdfType] = {
    val q = """PREFIX frame: <http://cambridgesemantics.com/ontologies/2008/07/OntologyService#>
              |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
              |SELECT ?cls
              |WHERE {
              |  ?frm frame:ontology <%s> .
              |  ?frm frame:class ?cls .
              |  ?cls rdfs:label ?label .
              |}""".stripMargin.format(ontology)
    anzo.serverQuery(null, null, ontologyService.asJava, q).getSelectResults.asScala.map(_.single[URI]).toSeq
  }

  def fieldsFor(rdfClass:RdfType)(implicit anzo:AnzoClient):Seq[RdfFieldDesc] = {
    val q = """PREFIX frame: <http://cambridgesemantics.com/ontologies/2008/07/OntologyService#>
              |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
              |SELECT ?ontProp ?range
              |WHERE {
              |  <%s> frame:frameProperty ?field .
              |  ?field frame:propertyRange ?range .
              |  ?field frame:ontologyProperty ?ontProp .
              |}""".stripMargin.format(rdfClass)
    anzo.serverQuery(null, null, ontologyService.asJava, q).getSelectResults.asScala.map(_.extract(r => r[URI] -> r[URI])).toSeq
  }

  def recordsFor(rdfClass:RdfType, fieldDescs:Seq[RdfFieldDesc], dataSets:Set[URI])(implicit anzo:AnzoClient) = {
    val neededFields = fieldDescs.filterNot(_._1 == RDF.TYPE)
    val fieldLabels = neededFields.indices.map("?field" + _).mkString(" ")
    val fieldQuery = neededFields.zipWithIndex.map{case ((f,_),idx) => s"\tOPTIONAL { ?id <$f> ?field$idx . }"}.mkString("\n")
    val q = s"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\nSELECT ?id $fieldLabels \nWHERE {\n\t?id rdf:type <$rdfClass> .\n$fieldQuery\n}"
    anzo.serverQuery(null, null, dataSets.asJava, q).getSelectResults.asScala.map { ps =>
      val id = ps.single[URI]
      val fieldMap = ps.extract{ i =>
        neededFields.map(_._1).iterator.zip(i).toMap
      }
      new RdfObject(rdfClass, id, fieldMap)
    }
  }

  def generalOnt(anzo: AnzoClient, ont: String, datasets: Set[String]) = {
    val dataset = EncodingUtils.uri("http://cambridgesemantics.com/semanticServices/OntologyService#Frames")
    val q:String = null

    val classUris = anzo.serverQuery(null, null, Set(dataset).asJava, q).getSelectResults.asScala.map {
      _.single[URI]
    }
    val classQuery = """PREFIX frame: <http://cambridgesemantics.com/ontologies/2008/07/OntologyService#>
                       |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                       |SELECT ?ontProp ?range
                       |WHERE {
                       |  <%s> frame:frameProperty ?field .
                       |  ?field frame:propertyRange ?range .
                       |  ?field frame:ontologyProperty ?ontProp .
                       |}""".stripMargin
    classUris foreach println
    val res = classUris.map(uri => uri -> anzo.serverQuery(null, null, Set(dataset).asJava, classQuery.format(uri)).getSelectResults.asScala.map(_.extract(r => r[URI] -> r[URI])).toSeq)
    val (actorUri, af) = res.head
    val actorFields = af.filterNot(_._1 == RDF.TYPE)
    val fields = actorFields.indices.map("?field" + _).mkString(" ")
    val fieldQueryPart = actorFields.zipWithIndex.map { case ((field, _), idx) => s"\tOPTIONAL {?actor <$field> ?field$idx .}" }.mkString("\n")
    val actorSlug = "<" + actorUri + ">"
    val genericQuery = s"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\nSELECT ?actor $fields \nWHERE {\n\t?actor rdf:type $actorSlug .\n$fieldQueryPart\n}"
    println(genericQuery)
    val res1 = anzo.serverQuery(null, null, datasets.map(EncodingUtils.uri).asJava, genericQuery).getSelectResults.asScala.head
    res1.extract { r => (r[URI], r[URI], r[String], r[DateTime], r[String], r[String]) }
  }

  implicit class UriStringContext(val sc:StringContext) extends AnyVal {
    def uri(args:Any*) = EncodingUtils.uri(sc.parts.mkString)
  }

  def main(args: Array[String]): Unit = {
    println("in main")
    implicit val anzo = new AnzoClient(T.conf)
    anzo.connect()
    val filmDataset = Set(uri"http://cambridgesemantics.com/datasets/film")


    val predsPerSubj = """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> SELECT ?t (COUNT(DISTINCT ?p) AS ?preds) { ?s ?p ?o . ?s rdf:type ?t } GROUP BY ?s ORDER BY ?preds"""

    val q = """SELECT  ?p (COUNT(DISTINCT ?o ) AS ?uniq ) (COUNT(?o) AS ?records) { ?s ?p ?o } GROUP BY ?p ORDER BY ?uniq"""

    val averageWidth =
      """PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        |SELECT ?t (AVERAGE(?count_preds) AS ?avg_width) {
        |  SELECT ?p (COUNT (DISTINCT ?p) AS ?count_preds) {
        |    ?s ?p ?o .
        |    ?s rdf:type ?t .
        |  }
        |  GROUP BY ?s
        |}
        |GROUP BY ?t
        |ORDER BY ?avg_width
      """.stripMargin

    val res = classesFor(uri"http://cambridgesemantics.com/ontologies/2009/08/Film").flatMap(cUri => recordsFor(cUri, fieldsFor(cUri), filmDataset))

    res foreach println

    //println(generalOnt(anzo, "http://cambridgesemantics.com/ontologies/2009/08/Film", Set("http://cambridgesemantics.com/datasets/film")))
    println("Done")
  }
}