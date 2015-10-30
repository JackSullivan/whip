package so.modernized.whip

import java.util.{Set => JSet}
import java.net.{URI => JURI}

import com.cambridgesemantics.anzo.unstructured.graphsummarization.PatternSolutionExtras
import com.cambridgesemantics.anzo.unstructured.graphsummarization.XMLUnapplicable._
import so.modernized.psl_scala.primitives.PSLUnapplicable._
import so.modernized.psl_scala.primitives.{PSLUnapplicable, PSLVar}
import so.modernized.whip.URIUniqueId._
import so.modernized.whip.sparql.QueryIterator
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

class TypedStandardPredicate[A, B](name:String, val uriType:AnzoURI, val domain:AnzoURI, val range:AnzoURI)(implicit aEv:PSLUnapplicable[A], bEv:PSLUnapplicable[B]) extends StandardPredicate(name, Array(aEv.argType, bEv.argType))


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

/*
class LazyResultList(iter:QueryIterator, varPos:Map[Variable, Int], val size:Int) extends ResultList {
  private val resStream = iter.flatten.toStream

  def get(resultNo: Int, `var`: Variable) = get(resultNo)(varPos(`var`))

  def get(resultNo: Int): Array[GroundTerm] = resStream(resultNo)

  val getArity = 2
}
*/

class SparqlResultList(varPos:Map[Variable, Int]) extends mutable.ArrayBuffer[Array[GroundTerm]] with ResultList {

  override def +=(elem: Array[GroundTerm]) = {
    assert(elem.length == 2)
    super.+=(elem)
  }

  override def get(resultNo: Int, `var`: Variable): GroundTerm = this(resultNo)(varPos(`var`))

  override def get(resultNo: Int): Array[GroundTerm] = this(resultNo)

  val getArity = 2
}

class PSLSparqlDataStore(protected[whip] val anzo:IAnzoClient, keyFields:Set[AnzoURI]) extends DataStore {
  protected[whip] val observedPredicates = mutable.HashSet[StandardPredicate]() //mutable.HashMap[AnzoURI, StandardPredicate]()
  protected[whip] val targetPredicates = mutable.HashSet[StandardPredicate]()
  protected[whip] val variables = mutable.HashMap[String, TypedVariable]()

  override def registerPredicate(predicate: StandardPredicate): Unit = {
    predicate match {
      case tp:TypedStandardPredicate[_,_] =>
        if(keyFields contains tp.uriType) {
          observedPredicates += tp
        } else {
          targetPredicates += tp
        }
      case s:StandardPredicate =>
        require(predicate.getArity == 2)
        Try(EncodingUtils.uri(predicate.getName)) match {
          case Success(uri) if keyFields contains uri => observedPredicates += predicate
          case Success(uri) => targetPredicates += predicate
          case Failure(f) => throw new IllegalArgumentException("Expected a uri for predicate name, got " + predicate.getName)
        }
    }

  }

  def registerTypedVariable(v:TypedVariable): Unit = { variables += v.name -> v }

  override def getRegisteredPredicates: JSet[StandardPredicate] = (observedPredicates ++ targetPredicates).asJava

  override def getUniqueID(key: Any): UniqueID = key match {
    case uri:AnzoURI => new URIUniqueId(uri)
    case jUri:JURI => new URIUniqueId(EncodingUtils.uri(jUri.toString))
    case str:String if Try(EncodingUtils.uri(str)).isSuccess => new URIUniqueId(EncodingUtils.uri(str))
    case otw => throw new IllegalArgumentException("Expected a uri or uri string, received " + otw.toString)
  }

  def getDatabase(datasets:Set[AnzoURI], ontology:AnzoURI=null) = new PSLSparqlDatabase(this, datasets, ontology, variables.toMap)

  override def getUpdater(predicate: StandardPredicate, partition: Partition): Updater = ???
  override def getInserter(predicate: StandardPredicate, partition: Partition): Inserter = ???
  override def deletePartition(partition: Partition): Int = ???
  override def getDatabase(write: Partition, read: Partition*): Database = ???
  override def getDatabase(write: Partition, toClose: JSet[StandardPredicate], read: Partition*): Database = ???
  override def close() {/*noOp*/}
  override def getNextPartition: Partition = ???
}

class PSLSparqlDatabase(private val datastore:PSLSparqlDataStore, private val datasets:Set[AnzoURI], private val ontology:AnzoURI, variableMap:Map[String,TypedVariable]) extends Database {
  private val anzo = datastore.anzo
  private val cache = new AtomCache(this)
  private val observed = datastore.observedPredicates
  private val target = datastore.targetPredicates

  def getAtom(p:Predicate, arguments:GroundTerm*) =
    Option(cache.getCachedAtom(new QueryAtom(p, arguments:_*))) match {
      case Some(res) => res
      case None => p match {
        case tp:TypedStandardPredicate[_,_] => // TODO should this work for non-typed predicates? nothing else will
          val Seq(PSLURI(s), PSLURI(o)) = arguments // TODO expand for other options
          val value = if(anzo.serverQuery(null, null, datasets.asJava, s"ASK { <$s> <${tp.uriType}> <$o> }").getAskResults) 1.0 else 0.0
          if(observed contains tp) {
            println("generating obs atom for " + (tp, arguments, value))
            cache.instantiateObservedAtom(tp, arguments.toArray, value, Double.NaN)
          } else if(target contains tp) {
            if(value > 0.0)
              println("generating rv atom for " + (tp, arguments, value))
            cache.instantiateRandomVariableAtom(tp, arguments.toArray, value, Double.NaN)
          } else {
            throw new IllegalArgumentException("Expected predicate to be registered as observed or target, but wasn't either")
          }
        case sp:SparqlPredicate =>
          if(!sp.isComputed) sp.precompute(this)
          cache.instantiateObservedAtom(sp, arguments.toArray, sp.computeValue(new ReadOnlyDatabase(this), arguments:_*), Double.NaN)
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

  def executeQuery(query:DatabaseQuery) = {
    val f = query.getFormula
    val atoms = f.getAtoms(mutable.Set.empty[Atom].asJava).asScala

    val projected = (query.getProjectionSubset.asScala.toSet ++
      f.collectVariables(new VariableTypeMap).asScala.keySet) --
      query.getPartialGrounding.asScala.keySet

    val projectedBindings = mutable.ArrayBuffer[Variable]()
    val whereClauses = atoms.map { a =>
      (a.getPredicate, a.getArguments) match {
        case (p:TypedStandardPredicate[_, _], Array(PSLVar(s), PSLVar(o))) if observed contains p =>
          projectedBindings += s
          projectedBindings += o
          s"\t?$s <${p.uriType}> ?$o ."
        case (p:TypedStandardPredicate[_, _], Array(PSLVar(s), PSLVar(o))) if target contains p =>
          val (sType, oType) = (s, o) match {
            case (PSLURIVar(su), PSLURIVar(ou)) => su.uriType -> ou.uriType
            case _ => p.domain -> p.range
          }
          projectedBindings += s
          projectedBindings += o
          Seq(s"\t?$s a <$sType> .",
            s"\t?$o a <$oType> .").mkString("\n")
        case (sp:SparqlPredicate, Array(PSLVar(s), PSLVar(o))) =>
          if(!sp.isComputed) {
            sp.precompute(this)
          }
          s"?$s <${sp.predicate}> ?$o ."
        case (p:StandardPredicate, ts) =>
          println ("observed " + observed + "\ntarget " + target)
          throw new IllegalArgumentException("Wasn't expecting " + (p, p.getClass, observed contains p, target contains p, ts.toSeq))
      }
    }.mkString("\n")
    val Q = s"SELECT ${projectedBindings.map(v => "?" + v.getName).toSet.mkString(" ")}\nWHERE {\n$whereClauses\n}"
    println(f)
    println(projected)
    println(Q)
    val res = new SparqlResultList(projectedBindings.zipWithIndex.toMap)
    val q = anzo.serverQuery(null, null, datasets.asJava, Q).getSelectResults.asScala.foreach { ps =>
      val m = ps.toMap
      res += projectedBindings.map(v => xml2Psl(m(v.getName))).toArray
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
        anzo.updateRepository(true)
      case otw => ???
    }
  }
}


