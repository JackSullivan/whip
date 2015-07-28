package so.modernized.whip.psl

import edu.umd.cs.psl.application.inference.MPEInference
import edu.umd.cs.psl.config.ConfigManager
import edu.umd.cs.psl.database.{ReadOnlyDatabase, DatabasePopulator, Partition, DataStore}
import edu.umd.cs.psl.database.rdbms.{RDBMSUniqueIntID, RDBMSDataStore}
import edu.umd.cs.psl.database.rdbms.driver.H2DatabaseDriver
import edu.umd.cs.psl.model.Model
import edu.umd.cs.psl.model.argument.{ArgumentType, GroundTerm, UniqueID}

import cc.factorie.app.strings.editDistance

import PSLDSL._
import FunctionConversions._
import edu.umd.cs.psl.model.predicate.StandardPredicate
import edu.umd.cs.psl.util.database.Queries
import org.apache.log4j.{PropertyConfigurator, Logger, Level}
import scala.collection.JavaConverters._
import scala.io.Source

object SamePersonExample {

  val logging = Logger.getLogger(this.getClass)

  def readFile(fileName:String) = Source.fromFile(fileName).getLines().flatMap {line =>
    line.split("""\s+""") match {
      case Array(x,y) => Some(x.toInt -> y.toInt)
      case otw => None
    }
  }

  def main(args:Array[String]): Unit = {
    //PropertyConfigurator.configure()
    logging.setLevel(Level.DEBUG)

    val config = ConfigManager.getManager.getBundle("basic-example")
    val levenshteinSim = {(s1:String, s2:String) =>
      val maxLen = math.max(s1.length, s2.length)
      if(maxLen == 0) {
        1.0
      } else {
        val sim = 1.0 - (editDistance(s1, s2).toDouble / maxLen.toDouble)
        if(sim > 0.5) sim else 0.0
      }
    }

    val defaultPath = sys.props("java.io.tmpdir") + "/basic-example"
    val dbPath = config.getString("dbpath", defaultPath)

    implicit val ds:DataStore = new RDBMSDataStore(new H2DatabaseDriver(H2DatabaseDriver.Type.Disk, dbPath, true), config)
    implicit val m:Model = new Model

    val Network = R[UniqueID, UniqueID]("Network")
    val Name = R[UniqueID, String]("Name")
    val Knows = R[UniqueID, UniqueID]("Knows")
    val SamePerson = new R[UniqueID with PartialFunctional, UniqueID with PartialFunctional]("SamePerson") with Symmetry
    val SameName = f("SameName", levenshteinSim)

    val snA:GroundTerm = ds.getUniqueID(1)
    val snB:GroundTerm = ds.getUniqueID(2)

    ((Network(v"A", snA) & Network(v"B", snB)
      & Name(v"A", v"X") & Name(v"B", v"Y")
      & SameName(v"X", v"Y")) >> SamePerson(v"A", v"B")).where(weight = 5.0)

    ((Network(v"A", snA) & Network(v"B", snB)
      & SamePerson(v"A", v"B")
      & Knows(v"A", v"Friend1") & Knows(v"B", v"Friend2")) >> SamePerson(v"Friend1", v"Friend2")).where(weight = 1.2)

    //(~SamePerson(v"A",v"B")).where(weight = 1.0)

    println(m)

    val evidencePart = new Partition(0)
    val insertName = ds.getInserter(Name, evidencePart)

    val networkA = Seq (
      (1, "John Braker"),
      (2, "Mr. Jack Ressing"),
      (3, "Peter Larry Smith"),
      (4, "Tim Barosso"),
      (5, "Jessica Pannillo"),
      (6, "Peter Smithsonian"),
      (7, "Miranda Parker")
    )

    val networkB = Seq(
      (11, "Johny Braker"),
      (12, "Jack Ressing"),
      (13, "PL S."),
      (14, "Tim Barosso"),
      (15, "J. Panelo"),
      (16, "Gustav Heinrich Gans"),
      (17, "Otto v. Lautern")
    )

    for((_,a) <- networkA;
        (_,b) <- networkB;
        sim = levenshteinSim(a,b)
        if sim > 0.0) {
      println(a,b,sim)
    }

    networkA foreach{case (i,n) => insertName.insert(id(i),n)}
    networkB foreach{case (i,n) => insertName.insert(id(i),n)}

    val dir = args(0)

    val insertNetwork = ds.getInserter(Network, evidencePart)

    val insertKnows = ds.getInserter(Knows, evidencePart)

    val networkData = readFile(dir + "sn_network.txt")
    val knowsData = readFile(dir + "sn_knows.txt")


    networkData foreach { case(i1, i2) =>
      insertNetwork.insert(id(i1), id(i2))
    }

    knowsData foreach { case(i1, i2) =>
      insertKnows.insert(id(i1), id(i2))
    }

    val targetPart = new Partition(1)
    val preds:Set[StandardPredicate] = Set(Network, Name, Knows)
    val db = ds.getDatabase(targetPart, preds.asJava, evidencePart)

    val population = Map(v"UserA" -> networkA.map{case (id,_) => ds.getUniqueID(id).asInstanceOf[GroundTerm]}.toSet.asJava,
                  v"UserB" -> networkB.map{case (id,_) => ds.getUniqueID(id).asInstanceOf[GroundTerm]}.toSet.asJava).asJava

    val populator = new DatabasePopulator(db)
    populator.populate(SamePerson(v"UserA", v"UserB"), population)
    populator.populate(SamePerson(v"UserB", v"UserA"), population)

    val inf = new MPEInference(m, db, config)
    inf.mpeInference()
    inf.close()

    val groundings = Queries.getAllAtoms(db, SamePerson).asScala

    groundings.toSeq.sortBy(-_.getValue) foreach { atom =>
      val Array(a,b) = atom.getArguments
      val ai = extract[RDBMSUniqueIntID](a).getID
      val bi = extract[RDBMSUniqueIntID](b).getID
      (networkA.toMap.get(ai),  networkB.toMap.get(bi)) match {
        case (Some(n1), Some(n2)) => println(n1, n2, atom.getValue)
        case otw => ()
      }
    }

  }

}

