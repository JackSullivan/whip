package so.modernized.whip

import java.util.{Set => JSet}
import java.net.{URI => JURI}

import com.cambridgesemantics.anzo.unstructured.graphsummarization.{SparqlPredicate, NodeSimilarity, PatternSolutionExtras}
import com.cambridgesemantics.anzo.unstructured.graphsummarization.XMLUnapplicable._
import org.apache.spark.sql.DataFrame
import so.modernized.psl_scala.primitives.PSLUnapplicable._
import so.modernized.psl_scala.primitives.{PSLUnapplicable, PSLVar}
import so.modernized.whip.URIUniqueId._
import so.modernized.whip.util._

import scala.util.{Failure, Success, Try}
import scala.collection.JavaConverters._
import scala.collection.mutable

import com.cambridgesemantics.anzo.utilityservices.common.EncodingUtils
import edu.umd.cs.psl.database.loading.{Updater, Inserter}
import edu.umd.cs.psl.database._
import edu.umd.cs.psl.model.argument._
import edu.umd.cs.psl.model.atom._
import edu.umd.cs.psl.model.predicate.{SpecialPredicate, FunctionalPredicate, Predicate, StandardPredicate}

import org.openanzo.client.IAnzoClient
import org.openanzo.rdf.{URI => AnzoURI, Statement, Value}

class TypedStandardPredicate[A, B](name:String, val uriType:AnzoURI)(implicit aEv:PSLUnapplicable[A], bEv:PSLUnapplicable[B]) extends StandardPredicate(name, Array(aEv.argType, bEv.argType))


/**
 * A Variable that is typed by the rdf:class of the arguments that it can take (determined by @uriType)
 */
case class TypedVariable(name:String, uriType:AnzoURI) extends Variable(name) {
  override def toString = name
}

object TypedVariable {
  def tv(name:String, uri:AnzoURI) = new TypedVariable(name, uri)
}

object PSLURIVar {
  def unapply(t:Term) = t match {
    case v:TypedVariable => Some(v)
    case _ => None
  }
}

class SparqlResultList(val arity:Int, varPos:Map[Variable, Int]) extends mutable.ArrayBuffer[Array[GroundTerm]] with ResultList {

  override def +=(elem: Array[GroundTerm]) = {
    assert(elem.length == arity)
    super.+=(elem)
  }

  override def get(resultNo: Int, `var`: Variable): GroundTerm = this(resultNo)(varPos(`var`))

  override def get(resultNo: Int): Array[GroundTerm] = this(resultNo)

  override def getArity = arity
}

class PSLSparqlDataStore(protected[whip] val anzo:IAnzoClient, keyFields:Set[AnzoURI]) extends DataStore {
  protected[whip] val observedPredicates = mutable.HashMap[AnzoURI, StandardPredicate]()
  protected[whip] val targetPredicates = mutable.HashMap[AnzoURI, StandardPredicate]()

  override def registerPredicate(predicate: StandardPredicate): Unit = {
    require(predicate.getArity == 2)
    Try(EncodingUtils.uri(predicate.getName)) match {
      case Success(uri) if keyFields contains uri => observedPredicates += uri -> predicate
      case Success(uri) => targetPredicates += uri -> predicate
      case Failure(f) => throw new IllegalArgumentException("Expected a uri for predicate name, got " + predicate.getName)
    }
  }

  override def getRegisteredPredicates: JSet[StandardPredicate] = (observedPredicates.values ++ targetPredicates.values).toSet.asJava

  override def getUniqueID(key: Any): UniqueID = key match {
    case uri:AnzoURI => new URIUniqueId(uri)
    case jUri:JURI => new URIUniqueId(EncodingUtils.uri(jUri.toString))
    case str:String if Try(EncodingUtils.uri(str)).isSuccess => new URIUniqueId(EncodingUtils.uri(str))
    case otw => throw new IllegalArgumentException("Expected a uri or uri string, received " + otw.toString)
  }

  def getDatabase(datasets:Set[AnzoURI], ontology:AnzoURI=null) = new PSLSparqlDatabase(this, datasets, ontology)

  override def getUpdater(predicate: StandardPredicate, partition: Partition): Updater = ???
  override def getInserter(predicate: StandardPredicate, partition: Partition): Inserter = ???
  override def deletePartition(partition: Partition): Int = ???
  override def getDatabase(write: Partition, read: Partition*): Database = ???
  override def getDatabase(write: Partition, toClose: JSet[StandardPredicate], read: Partition*): Database = ???
  override def close() {/*noOp*/}
  override def getNextPartition: Partition = ???
}

class PSLSparqlDatabase(private val datastore:PSLSparqlDataStore, private val datasets:Set[AnzoURI], private val ontology:AnzoURI) extends Database {
  private val anzo = datastore.anzo
  private val cache = new AtomCache(this)
  private val observed = datastore.observedPredicates.values.toSet
  private val target = datastore.targetPredicates.values.toSet

  override def getAtom(p: Predicate, arguments: GroundTerm*) =
    Option(cache.getCachedAtom(new QueryAtom(p, arguments:_*))) match {
      case Some(res) => res
      case None => p match {
        case sp:StandardPredicate =>
          val p = EncodingUtils.uri(sp.getName).toString
          arguments match {
            case Seq(PSLURI(s), PSLURI(o)) =>
              val value = if(anzo.serverQuery(null, null, datasets.asJava, s"ASK { <$s> <$p> <$o> }").getAskResults) 1.0 else 0.0
              if(observed contains sp) {
                cache.instantiateObservedAtom(sp, arguments.toArray, value, Double.NaN)
              } else if(target contains sp) {
                cache.instantiateRandomVariableAtom(sp, arguments.toArray, value, Double.NaN)
              } else {
                throw new IllegalArgumentException("Expected predicate to be registered as observed or target, but wasn't either")
              }
            case otw => throw new IllegalArgumentException("Expected a pair of URIs, got " + otw)
          }
        case fp:FunctionalPredicate => cache.instantiateObservedAtom(fp, arguments.toArray, fp.computeValue(new ReadOnlyDatabase(this), arguments:_*), Double.NaN)
      }
    }

  override def getRegisteredPredicates = datastore.getRegisteredPredicates

  override def getUniqueID(key: Any) = datastore.getUniqueID(key)

  override def getDataStore = datastore

  private val executeQ =
    """SELECT %s
      |WHERE {
      | %s
      |}""".stripMargin

  override def executeQuery(query: DatabaseQuery): ResultList = {
    val f = query.getFormula
    val atoms = f.getAtoms(mutable.Set.empty[Atom].asJava).asScala
    assert(atoms.forall(_.getArity == 2))

    val projected = (query.getProjectionSubset.asScala.toSet ++
      f.collectVariables(new VariableTypeMap).asScala.keySet) --
      query.getPartialGrounding.asScala.keySet

    /* I don't think this is right in the model
    val variableNotEqual = pairs(projected).map { case(v1, v2) =>
      s"FILTER(?$v1 != ?$v2) ."
    }.mkString("\n")
    */

    val projections = mutable.ArrayBuffer[Variable]()
    val whereClauses = atoms.map { a =>
      a.getPredicate match {
        case sp:TypedStandardPredicate[_,_] =>
          val p = sp.uriType
          a match {
            case obs:ObservedAtom => obs.getArguments match {
              case Array(PSLURI(s), PSLURI(o))   => s"<$s> <$p> <$o> ."
              case Array(PSLURIVar(sv), PSLURI(o))  =>
                projections += sv
                s"?$sv <$p> <$o> ."
              case Array(PSLURI(s), PSLURIVar(ov))  =>
                projections += ov
                s"<$s> <$p> ?$ov ."
              case Array(PSLURIVar(sv), PSLURIVar(ov)) =>
                projections += sv
                projections += ov
                s"?$sv <$p> ?$ov ."
              case otw => throw new IllegalArgumentException("Only binary atoms of URIs and variables are currently supported, received " + otw.toSeq)
            }
            case rv:RandomVariableAtom => rv.getArguments match {
              case Array(PSLURIVar(sv), PSLURIVar(ov)) =>
                projections += sv
                projections += ov
                Seq(s"?$sv a <${sv.uriType}> .",
                  s"?$ov a <${ov.uriType}> .",
                  s"?$sv <$p> $ov .").mkString("\n")
              case otw => throw new IllegalArgumentException("Only binary atoms of URIs and variables are currently supported, received " + otw.toSeq)
            }
          }
        case spq:SparqlPredicate => a.getArguments match {
          case Array(PSLURIVar(s), PSLURIVar(o)) =>
            if(!spq.isComputed) {
              spq.precompute(this, s.uriType, o.uriType)
            }
            s"?$s <${spq.predicate}> ?$o ."
        }
      }
    }.mkString("\n")

    val projectionString = projections.map(v => "?" + v.getName).toSet.mkString(" ")

    val preparedQ = executeQ.format(projectionString, whereClauses /*+ "\n" + variableNotEqual*/)
    println(preparedQ)

    val res = new SparqlResultList(2, projections.zipWithIndex.toMap)
    val q = anzo.serverQuery(null, null, datasets.asJava, preparedQ).getSelectResults.asScala.foreach { ps =>
      val m = ps.toMap
      res += projections.map(v => xml2Psl(m(v.getName))).toArray
    }
    res
  }


  override def close() {/*noOp*/}

  override def isClosed(predicate: StandardPredicate) = target contains predicate

  override def getAtomCache = cache

  override def commit(atom: RandomVariableAtom): Unit = {
    require(atom.getArity == 2)
    val p = EncodingUtils.uri(atom.getPredicate.getName)
    atom.getArguments match {
      case Array(PSLURI(s), PSLURI(o)) =>
        val stmt = new Statement(s, p, o)
        val stmtVal = new Statement(s, EncodingUtils.uri(p.toString +"_value"), xmlWrap(atom.getValue))
        val stmtConf = new Statement(s, EncodingUtils.uri(p.toString +"_confidence"), xmlWrap(atom.getConfidenceValue))
        anzo.add(stmt, stmtVal, stmtConf)
        anzo.commit()
      case otw => ???
    }
  }
}


