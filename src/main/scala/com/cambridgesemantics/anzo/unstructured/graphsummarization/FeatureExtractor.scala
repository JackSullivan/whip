package com.cambridgesemantics.anzo.unstructured.graphsummarization

import edu.umd.cs.psl.application.inference.MPEInference
import edu.umd.cs.psl.config.ConfigManager
import edu.umd.cs.psl.database.{ReadOnlyDatabase, DataStore}
import edu.umd.cs.psl.model.Model
import edu.umd.cs.psl.model.argument.{ArgumentType, GroundTerm}
import edu.umd.cs.psl.model.predicate.SpecialPredicate
import edu.umd.cs.psl.util.database.Queries
import AnzoURIExtras._
import org.openanzo.glitter.query.PatternSolution
import so.modernized.psl_scala.psl.everything._
import so.modernized.whip._
import so.modernized.whip.URIUniqueId._
import so.modernized.whip.util._
import VectorOps._
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
  //println(promQuery)

  val fieldTypeQuery = slurp(getClass.getResourceAsStream("./fieldTypes.rq"))
  .format(ontology.toString)
  //println(fieldTypeQuery)

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

  val linkMap = client.serverQuery(null, null, Set(ontologyFrame).asJava, fieldTypeQuery).getSelectResults.asScala.flatMap { ps =>
    val m = ps.toMap
    val XMLURI(cls) = m("cls")
    val XMLURI(ontProp) = m("ontProp")
    val XMLURI(range) = m("range")
    if(isLink(range)) {
      Some(cls -> (ontProp, range))
    } else {
      None
    }
  }.groupBy(_._1).mapValues(_.map(_._2))
    
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

      /*
      fields.collect {case (_, ProminenceStatistics(_,_,fieldType)) if isLink(fieldType) =>
        remainingFields ++= dataMap(fieldType).collect { case (field, ProminenceStatistics(_,prom,_)) if !fields.keySet.contains(field) =>
          Coords(fieldType, field) -> prom
        }
      }
      */

      val toPopulate = math.max(6 - prominentFields.size, 0)
      ontClass -> (remainingFields.filterNot(_._2 == 0.0).sortBy(-_._2).take(toPopulate).map(_._1) ++ prominentFields.keySet.map(f => Coords(ontClass, f)) ++ numericFields.keySet.map(f => Coords(ontClass, f))).toSet
    }
    res
  }

  def discreteEntropies(filter:Iterable[Coords]) = {
    val entropyQ = slurp(getClass.getResource("./discreteEntropy.rq")).format(filter.map(_.sparqlValues).mkString("\n"))
    //println(entropyQ)
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
  val exemplarUri = uri"http://graphsummary/exemplar"

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

    val weightsMap = keyFields.map { case (cls, coords) =>
        cls -> ext.discreteEntropies(coords)
          .map{ case (Coords(_, pred), (_, _, entropyProp)) => pred -> (1- entropyProp)}.toSeq.sortBy(_._1).map(_._2).toArray
    }
    val predicatesMap = keyFields.map {case (cls, coords) => cls -> coords.toSeq.map(_.predicate).sorted}

    implicit val ds = new PSLSparqlDataStore(anzo, keyFields.values.flatMap(_.map(_.predicate)).toSet)
    implicit val m = new Model


    //ext.linkMap foreach println

    val exemplars = weightsMap.map { case(cls, weights) =>
      val exemplar = new TypedStandardPredicate[AnzoURI, AnzoURI]("exemplar_" + cls.getLocalName, exemplarUri, cls, cls) //R[AnzoURI, AnzoURI](exemplarUri.toString, Functional, ArgNone, PredNone)
      ds registerPredicate exemplar
      val preds = predicatesMap(cls)
      val links = ext.linkMap.getOrElse(cls, Iterable.empty)
      val similar = new NodeSimilarity(anzo, Set(fedataset), preds, cls, cls, weights) //f(cls.toString, createSimilarityFn(anzo, Set(fedataset), preds, weights))
      val A = new TypedVariable("A", cls)
      val B = new TypedVariable("B", cls)
      val C = new TypedVariable("C", cls)
      val D = new TypedVariable("D", cls)
      Seq(A, B, C, D) foreach ds.registerTypedVariable
      links.foreach { case (field, value) =>
        // in the original paper the link feature is symmetric, but in our case they are not
        //val link = R[AnzoURI, AnzoURI](field.toString, ArgNone, ArgNone, new Symmetry {})
        val link = new TypedStandardPredicate[AnzoURI, AnzoURI]("link_" + field.getLocalName, field, cls, value) //R[AnzoURI, AnzoURI](field.toString)
        val P = new TypedVariable("P", value)
        ds registerPredicate link
        ds registerTypedVariable P

        (exemplar(A, B) >> similar(A, B)).where(1.0)
        (exemplar(A, B) >> exemplar(B, B)).where(1.0)
        ((link(A, P) & link(C, P) & exemplar(A, D)) >> exemplar(C, D)).where(1.0)
        ((link(A, P) & exemplar(A, C) & exemplar(D, C)) >> link(D, P)).where(1.0)

        exemplar(A, B).where(-1)

      }
      cls -> exemplar
    }

    val db = ds.getDatabase(ext.dataSets)

    val inf = new MPEInference(m, db, ConfigManager.getManager.getBundle(""))
    inf.mpeInference()
    inf.close()

    exemplars foreach { case(cls, exemplar) =>
      println(cls)
        Queries.getAllAtoms(db, exemplar).asScala.foreach(a => println("\t" + a))
    }

    println("done")
  }

}

class NodeSimilarity(val anzo:IAnzoClient, val dataSets:Set[AnzoURI], predicates:Seq[AnzoURI], val domain:AnzoURI, val range:AnzoURI, val weights:Array[Double])
  extends SpecialPredicate("#FieldOverlap", Array(ArgumentType.UniqueID, ArgumentType.UniqueID))
  with AggregateSparqlPredicate {

  private val predMap = predicates.zipWithIndex.toMap
  private def extractRowId(ps:PatternSolution) = {
    val m = ps.toMap
    val XMLURI(s1) = m("s1")
    val XMLURI(s2) = m("s2")
    s1 -> s2
  }

  val aggregateQuery: String = slurp(getClass.getResourceAsStream("./aggFeatureVector.rq"))

  def cutoff(a:PatternSolution, b:PatternSolution) = extractRowId(a) != extractRowId(b)

  val queryPopulators = Seq(predicates.map("\t\t\t(<" + _ + ">)").mkString("\n"))

  def agg(as:Seq[PatternSolution]):((AnzoURI, AnzoURI), Array[Double]) =
    as.foldLeft((null.asInstanceOf[AnzoURI], null.asInstanceOf[AnzoURI]) -> new Array[Double](predMap.size)) {
      case (((_), arr), ps) =>
        val XMLURI(p) = ps.toMap("p")
        arr(predMap(p)) = Option(ps.getBinding("val")).collectFirst(XMLInt).getOrElse(0).toDouble
        extractRowId(ps) -> arr
    }

  private val featureQ = slurp(getClass.getResourceAsStream("./featureVector.rq"))
  override def computeUnderlying(db: ReadOnlyDatabase, args: GroundTerm*): Double = {
    val Seq(PSLURI(s1), PSLURI(s2)) = args
    val q = featureQ.format(s1, s2, queryPopulators.head)
    val res = anzo.serverQuery(null, null, dataSets.asJava, q).getSelectResults.asScala
    val feats = res
      .zipWithIndex.foldLeft(new Array[Double](predicates.size)) { case (arr,  (ps, idx)) =>
      arr(idx) = Option(ps.getBinding("val")).collectFirst(XMLInt).getOrElse(0).toDouble
      arr
    }
    // we should be able to do this since feature vector query orders by p, and weights should be ordered by p
    (feats dot weights)/feats.length
  }
}
