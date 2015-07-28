package so.modernized.whip.psl

import edu.umd.cs.psl.application.inference.MPEInference
import edu.umd.cs.psl.config.ConfigManager
import edu.umd.cs.psl.database.{DatabasePopulator, Partition, DataStore}
import edu.umd.cs.psl.database.rdbms.RDBMSDataStore
import edu.umd.cs.psl.database.rdbms.driver.H2DatabaseDriver
import edu.umd.cs.psl.model.Model
import edu.umd.cs.psl.model.argument.{GroundTerm, UniqueID}

import cc.factorie.app.strings.editDistance

import PSLDSL._
import FunctionConversions._
import edu.umd.cs.psl.model.predicate.StandardPredicate
import edu.umd.cs.psl.ui.loading.InserterUtils
import edu.umd.cs.psl.util.database.Queries
import scala.collection.JavaConverters._
import scala.io.Source

object SamePersonExample {

  def readFile(fileName:String) = Source.fromFile(fileName).getLines().flatMap {line =>
    line.split("""\s+""") match {
      case Array(x,y) => Some(x.toInt -> y.toInt)
      case otw => None
    }
  }

  def main(args:Array[String]): Unit = {

    val config = ConfigManager.getManager.getBundle("basic-example")

    val defaultPath = sys.props("java.io.tmpdir") + "/basic-example"
    val dbPath = config.getString("dbpath", defaultPath)

    implicit val ds:DataStore = new RDBMSDataStore(new H2DatabaseDriver(H2DatabaseDriver.Type.Disk, dbPath, true), config)
    implicit val m:Model = new Model

    val Network = R[UniqueID, UniqueID]("Network")
    val Name = R[UniqueID, String]("Name")
    val Knows = R[UniqueID, UniqueID]("Knows")
    val SamePerson = new R[UniqueID with Functional, UniqueID]("SamePerson") with Symmetry
    val SameName = f("SameName", {(s1:String, s2:String) => editDistance(s1,s2).toDouble})

    val snA:GroundTerm = ds.getUniqueID(1)
    val snB:GroundTerm = ds.getUniqueID(2)

    val rule1 = (Network(v"A", snA) & Network(v"B", snB) & Name(v"A", v"X") & Name(v"B", v"Y") & SameName(v"X", v"Y")) >> SamePerson(v"A", v"B")
    rule1.where(weight = 5.0)
    val rule2 = (Network(v"A", snA) & Network(v"B", snB) & SamePerson(v"A", v"B") & Knows(v"A", v"Friend1") & Knows(v"B", v"Friend2")) >> SamePerson(v"Friend1", v"Friend2")
    rule2.where(weight = 3.2)

    (~SamePerson(v"A",v"B")).where(weight = 1)

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

    networkA foreach{case (id,n) => insertName.insert(new Integer(id),n)}
    networkB foreach{case (id,n) => insertName.insert(new Integer(id),n)}

    val dir = args(0)

    val insertNetwork = ds.getInserter(Network, evidencePart)

    val insertKnows = ds.getInserter(Knows, evidencePart)

    val networkData = readFile(dir + "sn_network.txt")
    val knowsData = readFile(dir + "sn_knows.txt")

    networkData foreach { case(i1, i2) =>
      insertNetwork.insert(new Integer(i1), new Integer(i2))
    }
    knowsData foreach { case(i1, i2) =>
      insertKnows.insert(new Integer(i1), new Integer(i2))
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


    Queries.getAllAtoms(db, SamePerson).asScala foreach { atom =>
      println(atom.toString + "\t" + "%.3f".format(atom.getValue))
    }

  }

}

