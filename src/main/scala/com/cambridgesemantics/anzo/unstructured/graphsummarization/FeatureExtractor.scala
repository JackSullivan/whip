package com.cambridgesemantics.anzo.unstructured.graphsummarization

import Readable._
import XMLUnapplicable._
import org.openanzo.client.{AnzoClient, IAnzoClient}
import org.openanzo.rdf.{URI => AnzoURI}
import org.openanzo.test.AbstractTest
import scala.collection.JavaConverters._
import scala.collection.mutable

case class Coords(ontClass:AnzoURI, predicate:AnzoURI) extends Ordered[Coords] {
  override def toString = s"Coords(${ontClass.getLocalName}, ${predicate.getLocalName})"

  override def compare(that: Coords) = this.ontClass compareTo that.ontClass match {
    case 0 => this.predicate compareTo that.predicate
    case otw => otw
  }
}
case class ProminenceStatistics(populatedRate:Double, prominence:Double, range:AnzoURI)

trait FeatureExtractor {
  def client:IAnzoClient
  def ontology:AnzoURI
  def dataSets:Set[AnzoURI]

  val ontologyFrame = uri"http://cambridgesemantics.com/semanticServices/OntologyService#Frames"

  val promQuery = slurp(getClass.getResourceAsStream("/calculateProminence.rq"))
  println(promQuery)

  val fieldTypeQuery = slurp(getClass.getResourceAsStream("/fieldTypes.rq")).format(ontology.toString)
  println(fieldTypeQuery)

  implicit def comb(a:(Int, Int, Int, Double, Double),b:AnzoURI) = ProminenceStatistics(a._4, a._5, b)
  val combMap = client.serverQuery(null, null, dataSets.asJava, promQuery).getSelectResults.asScala.map { ps =>
    val List(XMLURI(t), XMLURI(p), XMLInt(uniqs), XMLInt(numRecs), XMLInt(numSubjs), XMLDouble(popRate), XMLDouble(prom)) = ps.values
    Coords(t,p) -> (uniqs,numRecs, numSubjs, popRate, prom)
  }.toMap |+| client.serverQuery(null, null, Set(ontologyFrame).asJava, fieldTypeQuery).getSelectResults.asScala.map { ps =>
    val List(XMLURI(cls), XMLURI(fld), XMLURI(range)) = ps.values
    Coords(cls,fld) -> range
  }.toMap


  def prominentCategories(cands:Map[Coords, ProminenceStatistics]) = {
    val dataMap = cands.toSeq.groupBy(_._1.ontClass).mapValues(_.groupBy(_._1.predicate).mapValues(_.head._2))
    val res = dataMap.map { case (ontClass, fields) =>

      val prominentFields = fields.collect { case tup@(field, ProminenceStatistics(_,prom,_)) if prom > 0 => tup }

      val numericFields = fields.filter{case (_, ProminenceStatistics(_,_,fieldType)) => isNumeric(fieldType)}


      val remainingFields = new mutable.ArrayBuffer[(AnzoURI, Double)]
      remainingFields ++= fields.collect { case (k, ProminenceStatistics(_,prom,_)) if (prominentFields.keySet ++ numericFields.keySet).contains(k) =>
        k -> prom
      }

      fields.collect {case (_, ProminenceStatistics(_,_,fieldType)) if isLink(fieldType) =>
        remainingFields ++= dataMap(fieldType).collect { case (field, ProminenceStatistics(_,prom,_)) if !fields.keySet.contains(field) =>
          field -> prom
        }
      }

      val toPopulate = math.max(6 - prominentFields.size, 0)
      ontClass -> (remainingFields.filterNot(_._2 == 0.0).sortBy(-_._2).take(toPopulate).map(_._1) ++ prominentFields.keySet ++ numericFields.keySet)
    }
    res
  }

}

object T extends AbstractTest() {
  def conf = getSystemClientConfiguration
}

object FeatureExtractorTest {
  def main(args:Array[String]) {
    implicit val anzo = new AnzoClient(T.conf)
    anzo.connect()
    val fedataset = uri"http://cambridgesemantics.com/datasets/film"
    val feontology = uri"http://cambridgesemantics.com/ontologies/2009/08/Film"

    val ext = new FeatureExtractor {
      val client = anzo
      val ontology = feontology
      val dataSets = Set(fedataset)
    }
  }
}
