package com.cambridgesemantics.anzo.unstructured.graphsummarization

import so.modernized.whip.util._
import Readable._
import XMLUnapplicable._
import org.openanzo.test.AbstractTest
import org.openanzo.client.{IAnzoClient, AnzoClient}
import org.openanzo.rdf.vocabulary.XMLSchema
import org.openanzo.rdf.{URI => AnzoURI}

import scala.collection.JavaConverters._
import scala.collection.mutable

object T extends AbstractTest {
  val conf = getSystemClientConfiguration
}

case class Coords(ontClass:AnzoURI, predicate:AnzoURI) extends Ordered[Coords] {
  override def toString = s"Coords(${ontClass.getLocalName}, ${predicate.getLocalName})"

  override def compare(that: Coords) = this.ontClass compareTo that.ontClass match {
    case 0 => this.predicate compareTo that.predicate
    case otw => otw
  }

  lazy val sparqlValues = s"(<${ontClass.toString}> <${predicate.toString}>)"
}

case class ProminenceStatistics(populatedRate:Double, prominence:Double, range:AnzoURI)

trait FeatureExtractor {
  def client:IAnzoClient
  def ontology:AnzoURI
  def dataSets:Set[AnzoURI]
  
  val ontologyFrame = uri"http://cambridgesemantics.com/semanticServices/OntologyService#Frames"

  val promQuery = slurp(getClass.getResourceAsStream("./calculateProminence.rq"))
  println(promQuery)

  val fieldTypeQuery = slurp(getClass.getResourceAsStream("./fieldTypes.rq"))
  .format(ontology.toString)
  println(fieldTypeQuery)

  implicit def comb(a:Option[(Int, Int, Int, Double, Double)], b:Option[AnzoURI]) = {
    val (popRate, prom) = a match {
      case Some((_,_,_,pr, p)) => pr -> p
      case None => 0.0 -> 0.0
    }
    val range = b match {
      case Some(uri) => uri
      case None => XMLSchema.STRING
    }
    ProminenceStatistics(popRate, prom, range)
  } 
    
  val combMap = client.serverQuery(null, null, dataSets.asJava, promQuery).getSelectResults.asScala.map { ps =>
    val m = ps.toMap
    val XMLURI(t) = m("t")
    val XMLURI(p) = m("p")
    val XMLInt(uniqs) = m("uniqs")
    val XMLInt(numRecs) = m("numRecs")
    val XMLInt(numSubjs) = m("numSubjs")
    val XMLDouble(popRate) = m("populatedRate")
    val XMLDouble(prom) = m ("prom")
    Coords(t,p) -> (uniqs,numRecs, numSubjs, popRate, prom)
  }.toMap |+| client.serverQuery(null, null, Set(ontologyFrame).asJava, fieldTypeQuery).getSelectResults.asScala.map { ps =>
    val m = ps.toMap
    val XMLURI(cls) = m("cls")
    val XMLURI(ontProp) = m("ontProp")
    val XMLURI(range) = m("range")
    Coords(cls,ontProp) -> range
  }.toMap


  def prominentCategories(cands:Map[Coords, ProminenceStatistics]) = {
    val dataMap = cands.toSeq.groupBy(_._1.ontClass).mapValues(_.groupBy(_._1.predicate).mapValues(_.head._2))
    val res = dataMap.map { case (ontClass, fields) =>

      val prominentFields = fields.collect { case tup@(field, ProminenceStatistics(_,prom,_)) if prom > 0 => tup }

      val numericFields = fields.filter{case (_, ProminenceStatistics(_,_,fieldType)) => isNumeric(fieldType)}


      val remainingFields = new mutable.ArrayBuffer[(Coords, Double)]
      remainingFields ++= fields.collect { case (k, ProminenceStatistics(_,prom,_)) if (prominentFields.keySet ++ numericFields.keySet).contains(k) =>
        Coords(ontClass, k) -> prom
      }

      fields.collect {case (_, ProminenceStatistics(_,_,fieldType)) if isLink(fieldType) =>
        remainingFields ++= dataMap(fieldType).collect { case (field, ProminenceStatistics(_,prom,_)) if !fields.keySet.contains(field) =>
          Coords(fieldType, field) -> prom
        }
      }

      val toPopulate = math.max(6 - prominentFields.size, 0)
      ontClass -> (remainingFields.filterNot(_._2 == 0.0).sortBy(-_._2).take(toPopulate).map(_._1) ++ prominentFields.keySet.map(f => Coords(ontClass, f)) ++ numericFields.keySet.map(f => Coords(ontClass, f))).toSet
    }
    res
  }

  def discreteEntropies(filter:Iterable[Coords]) = {
    val entropyQ = slurp(getClass.getResource("./discreteEntropy.rq")).format(filter.map(_.sparqlValues).mkString("\n"))
    println(entropyQ)
    val entropies = client.serverQuery(null, null, dataSets.asJava, entropyQ).getSelectResults.asScala.map { ps =>
      val m = ps.toMap
      val XMLURI(cls) = m("cls")
      val XMLURI(p) = m("p")
      val XMLDouble(entropy) = m("log_prob")
      val XMLDouble(maxEnt) = m("max_ent")
      val XMLDouble(entProp) = m("ent_prop")
      Coords(cls, p) -> (entropy, maxEnt, entProp)
    }.toMap
    entropies
  }

  def jointEntropies(filter:Iterable[Coords]) = {
    pairs(filter) foreach println
    val values = pairs(filter).map { case (Coords(c1, p1), Coords(c2, p2)) =>
      s"(<$c1> <$p1> <$c2> <$p2>)"
    }.mkString("\n")
    implicit def comb(a:Option[Double], b:Option[Double]) = {
      val ent = a.getOrElse(0.0)
      val maxEnt = b.getOrElse(1.0)
      (ent, maxEnt, ent/maxEnt)
    }
    val jointEntQ = slurp(getClass.getResource("./jointEntropy.rq")).format(values)
    val maxEntQ = slurp(getClass.getResource("./jointMaxEnt.rq")).format(values)
    client.serverQuery(null, null, dataSets.asJava, jointEntQ).getSelectResults.asScala.map { ps =>
      val m = ps.toMap
      val XMLURI(cls1) = m("cls1")
      val XMLURI(p1) = m("p1")
      val XMLURI(cls2) = m("cls2")
      val XMLURI(p2) = m("p2")
      val XMLDouble(ent) = m("ent")
      (Coords(cls1, p1),  Coords(cls2, p2)) -> ent
    }.toMap |+| client.serverQuery(null, null, dataSets.asJava, maxEntQ).getSelectResults.asScala.map { ps =>
      val m = ps.toMap
      val XMLURI(cls1) = m("cls1")
      val XMLURI(p1) = m("p1")
      val XMLURI(cls2) = m("cls2")
      val XMLURI(p2) = m("p2")
      val XMLDouble(maxEnt) = m("max_ent")
      (Coords(cls1, p1),  Coords(cls2, p2)) -> maxEnt
    }.toMap
  }
}

object FeatureExtractorTest {
  def main(args:Array[String]) {
    implicit val anzo = new AnzoClient(T.conf)
    anzo.connect()
    val fedataset = uri"http://cambridgesemantics.com/datasets/film"
    val feontology = uri"http://cambridgesemantics.com/ontologies/2009/08/Film"
    
    val ext = new FeatureExtractor {
      override def client = anzo 
      override def ontology = feontology 
      override def dataSets = Set(fedataset)
    }
    
    val keyFields = ext.prominentCategories(ext.combMap)

    keyFields.foreach { case(ontC, fields) =>
      println(ontC)
      fields.foreach { f =>
        println("\t" + f)
      }
    }

    //keyFields.head._2 foreach println
    ext.discreteEntropies(keyFields.head._2)
    val res = ext.jointEntropies(keyFields.head._2)

    res foreach println


    println("done")
  }
}
/*
class OverlapFeatureFunction(val anzo:IAnzoClient, dataSet:AnzoURI, predicates:Seq[AnzoURI], weights:Array[Double]) extends ExternalFunction {
  override def getValue(db: ReadOnlyDatabase, args: GroundTerm*): Double = {
    val Seq(id1:URIUniqueId, id2:URIUniqueId) = args

    val featureQuery = slurp(getClass.getResourceAsStream("./featureVector.rq")).format(id1.toString, id2.toString, predicates.map("<" + _ + ">").mkString("\n"))
    val feats = anzo.serverQuery(null, null, Set(dataSet).asJava, featureQuery).getSelectResults.asScala
      .zipWithIndex.foldLeft(new Array[Double](predicates.size)) { case (arr,  (ps, idx)) =>
        arr(idx) = XMLInt.unapply(ps.getBinding("val")).get.toDouble
        arr
    }
    feats dot weights
  }

  val getArity = 2

  val getArgumentTypes = Array(ArgumentType.UniqueID, ArgumentType.UniqueID)
}
*/
