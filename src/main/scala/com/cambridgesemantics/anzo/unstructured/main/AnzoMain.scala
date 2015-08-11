package com.cambridgesemantics.anzo.unstructured.main

import java.util.Properties
import org.openanzo.client.{AnzoClientDictionary, AnzoClientConfigurationFactory, AnzoClient}
import org.openanzo.combus.{CombusDictionary, CombusProperties}
import org.openanzo.services.ServicesProperties
import com.cambridgesemantics.anzo.utilityservices.common.EncodingUtils
import scala.collection.JavaConverters._
import org.openanzo.test.AbstractTest
import org.openanzo.glitter.query.PatternSolution
import org.openanzo.rdf.{Variable, BlankNode, Literal, MemTypedLiteral, StoredTypedLiteral}
import com.cambridgesemantics.anzo.datasource.bigdata.BDTypedLiteral
import org.openanzo.rdf.vocabulary.XMLSchema
import org.openanzo.rdf.URI
import so.modernized.whip.sparql._

sealed trait ValueContainer[T] {
  def get:T
}
case class StringContainer(get:String) extends ValueContainer[String]
case class BooleanContainer(get:Boolean) extends ValueContainer[Boolean]
case class IntContainer(get:Int) extends ValueContainer[Int]
case class UriContainer(get:URI) extends ValueContainer[URI]


object AnzoMain {

  def typeLit(lit:Literal) = {
    println(lit)
    val dataType = lit match {
      case bd:BDTypedLiteral => bd.getDatatypeURI
      case mem:MemTypedLiteral => mem.getDatatype
      case str:StoredTypedLiteral => str.getDatatypeURI
    }
    println(dataType)
    val cont = dataType match {
      case XMLSchema.STRING => StringContainer(lit.getNativeValue.asInstanceOf[String])
      case XMLSchema.BOOLEAN => BooleanContainer(lit.getNativeValue.asInstanceOf[java.lang.Boolean].booleanValue())
      case XMLSchema.INTEGER => IntContainer(lit.getNativeValue.asInstanceOf[java.lang.Integer].intValue())
      case _ => throw new Error(dataType.toString())
    }
    cont.asInstanceOf[ValueContainer[_]]
  }

  /*
  lit match {
  case bool if lit.isBoolean()  => bool.getNativeValue.asInstanceOf[java.lang.Boolean].booleanValue
  case date if lit.isDateTime() => date.getNativeValue.asInstanceOf[java.util.Date]
  case num  if lit.isNumeric()  => num.getNativeValue.asInstanceOf
  case str  if lit.isString()   => str.getNativeValue.asInstanceOf[String]
}
*
*/

  /*
  def clientConf(username:String, password:String) = {
    val properties = new Properties
    CombusProperties.setHost(properties, "localhost")
    CombusProperties.setPort(properties, 8946)
    CombusProperties.setUseSsl(properties, false)
    ServicesProperties.setUser(properties, "default")
    ServicesProperties.setPassword(properties, "123")
    properties.put("http.port", "8946")

    val configGraph = AnzoClientConfigurationFactory.createJMSConfiguration(username, password, CombusProperties.getHost(properties), CombusProperties.getPort(properties), CombusProperties.getUseSsl(properties))
    AnzoClientDictionary.setUseCometd(configGraph, false)
    CombusDictionary.setUseSsl(configGraph, false)
    configGraph.put("http.port", "8946")
    ServicesProperties.setTimeout(configGraph, 9500000)
    AnzoClientConfigurationFactory.configureNonPersistedClient(configGraph)
    configGraph
  }
  */

  object T extends AbstractTest() {
    def conf = getSystemClientConfiguration
  }

  def resultMap(sol:PatternSolution) = (0 until sol.size()).map { idx =>
    val bind = sol.getBinding(idx) match {
      case v:Variable => v.getName
      case bn:BlankNode => bn.getLabel
    }
    val value = sol.getBinding(bind) match {
      case lit:Literal => typeLit(lit)
      case uri:URI => UriContainer(uri)
    }
    bind -> value.asInstanceOf[ValueContainer[_]]
  }

  def movie(anzo:AnzoClient) {
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
    anzo.serverQuery(null, null, Set(dataset).asJava, query).getSelectResults.asScala.take(5).foreach (res => println(resultMap(res)))

  }

  def movieOnt(anzo:AnzoClient) = {
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

  case class Field(range:URI, property:URI)

  def generalOnt(anzo:AnzoClient, ont:String, datasets:Set[String]) = {
    val dataset = EncodingUtils.uri("http://cambridgesemantics.com/semanticServices/OntologyService#Frames")

    val q = """PREFIX frame: <http://cambridgesemantics.com/ontologies/2008/07/OntologyService#>
              |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
              |SELECT ?cls
              |WHERE {
              |  ?frm frame:ontology <%s> .
              |  ?frm frame:class ?cls .
              |  ?cls rdfs:label ?label .
              |}""".stripMargin.format(ont)
    println(q)
    val classUris = anzo.serverQuery(null, null, Set(dataset).asJava, q).getSelectResults.asScala.map{ _.single[URI]}
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
    val actorFields = af.init
    val fields = actorFields.indices.map("?field" + _).mkString(" ")
    val fieldQueryPart = actorFields.zipWithIndex.map{case ((field, _), idx) => s"\tOPTIONAL {?actor <$field> ?field$idx .}"}.mkString("\n")
    val actorSlug = "<" + actorUri + ">"
    val genericQuery = s"PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\nSELECT ?actor $fields \nWHERE {\n\t?actor rdf:type $actorSlug .\n$fieldQueryPart\n}"
    println(genericQuery)
    val res1 = anzo.serverQuery(null, null, datasets.map(EncodingUtils.uri).asJava, genericQuery).getSelectResults.asScala.head
    res1.extract { r => (r[URI], r[URI], r[String], r[java.util.Date], r[String], r[String]) }
  }


  def main(args:Array[String]): Unit = {
    println("in main")
    val anzo = new AnzoClient(T.conf)
    anzo.connect()
    val m1 = EncodingUtils.uri("http://cambridgesemantics.com/demo/film/movie_the_wind_will_carry_us")
    val m2 = EncodingUtils.uri("http://cambridgesemantics.com/demo/film/movie_finding_nemo")
    val Movie = EncodingUtils.uri("http://cambridgesemantics.com/ontologies/2009/08/Film#Movie")
    val filmLDS = EncodingUtils.uri("http://cambridgesemantics.com/linkedDataSets/film")
    /*
    anzo.serverFind(null, null, null, m1, m2).asScala.map { statement =>
      println(statement)
    }
    */

    val places = Seq("<http://cambridgesemantics.com/City/4C9D113873A24FD6ABC5687C4D320E61>", "<http://cambridgesemantics.com/City/F388B02E08F841A8972C78EBC5863783>", "<http://cambridgesemantics.com/City/1FEBA538942749E9ABA456C04EE70FFC>", "<http://cambridgesemantics.com/City/6DBFCDE99DE94CA1AF831FC6CB6FD14C>")
      .map("(" + _ + ")").mkString("\n")
    /*
      StringBuilder sb = new StringBuilder();
      sb.append("PREFIX geo: <http://cambridgesemantics.com/ontologies/AnzoGeospatialOntology#>\n\n");
      sb.append("SELECT ?placeName ?place ?lat ?lon ?pop ");
      for (int i = 0; i < parentPreds.length; i++) {
          sb.append("?parent" + i + " ");
      }
      sb.append(" WHERE { ");
      sb.append(" ?place geo:placeName ?placeName . ");
      sb.append("OPTIONAL { ?place geo:geographicLocation ?loc . ?loc geo:lat ?lat . ?loc geo:long ?lon . } ");
      sb.append("OPTIONAL { ?place geo:population ?pop . } ");
      for (int i = 0; i < parentPreds.length; i++) {
          sb.append("OPTIONAL { ?place " + parentPreds[i] + " ?parent" + i + " . } ");
      }
      sb.append(" } ");
      sb.append(" VALUES (?place){ ");
      for (URI place : places) {
          sb.append(" (<" + place + ">) ");
      }
      sb.append(" } ");
      URI geoRegistryUri = EncodingUtils.uri("http://cambridgesemantics.com/dataSets/59815C9130384DA5980F44AC223F74AB/dataset");
      QueryResults results = pipeline.getAnzoClient(geoDatasource).serverQuery(null, null, Collections.singleton(geoRegistryUri), sb.toString());
      */
    val q = """PREFIX geo: <http://cambridgesemantics.com/ontologies/AnzoGeospatialOntology#>
              |
              |SELECT ?placeName ?place ?lat ?lon ?pop
              |WHERE {
              | ?place geo:placeName ?placeName .
              |OPTIONAL { ?place geo:geographicLocation ?loc . ?loc geo:lat ?lat . ?loc geo:long ?lon . }
              |OPTIONAL { ?place geo:population ?pop . } }""".stripMargin
    /*
                      |}
                      | VALUES (?place) {
                      | %s
                      |}""".stripMargin.format(places)
      */
    /*
val query = """PREFIX film: <http://cambridgesemantics.com/ontologies/2009/08/Film#>
    |SELECT ?title ?rank
    |WHERE {
    |  ?movie a film:Movie.
    |  ?movie film:title   ?title.
    |  ?movie film:ranking ?rank.
    |}""".stripMargin
    *
    */
    //println(q)
    //val geoUri = EncodingUtils.uri("http://cambridgesemantics.com/dataSets/59815C9130384DA5980F44AC223F74AB/dataset");
    //val res = anzo.serverQuery(null, null, Set(geoUri).asJava, q)
    //res.getSelectResults.asScala.take(5) foreach println
    //movie(anzo)
    //movieOnt(anzo)
    println(generalOnt(anzo, "http://cambridgesemantics.com/ontologies/2009/08/Film", Set("http://cambridgesemantics.com/datasets/film")))
    println("Done")
  }


