package so.modernized

import java.io.File

import and.useful.Readable._

import scala.collection.mutable

/**
 * Created by johnsullivan on 9/15/15.
 */
object TripleGV {

  val inCluster = "<http://cambridgesemantics.com/ontologies/2011/07/DocumentMetadata#inCluster>"
  val partOfCluster = "<http://cambridgesemantics.com/ontologies/2011/07/DocumentMetadata#partOfSupercluster>"
  val containsSubCluster = "<http://cambridgesemantics.com/ontologies/2011/07/DocumentMetadata#containsSubcluster>"

  val predMap = new mutable.HashMap[String, mutable.ArrayBuffer[(String, String)]]
  predMap += inCluster -> mutable.ArrayBuffer[(String, String)]()
  predMap += partOfCluster -> mutable.ArrayBuffer[(String, String)]()
  predMap += containsSubCluster -> mutable.ArrayBuffer[(String, String)]()

  def main(args:Array[String]): Unit = {
    splitWhitespace(new File(args(0))).foreach {
      case Array(subj, pred, obj, grph) =>
        if(predMap contains pred) {
          println(subj -> obj)
          val arr = predMap(pred)
          arr += subj -> obj
          predMap += pred -> arr
        }
      case otw => ()//println(otw.toSeq)
    }

    predMap(containsSubCluster) foreach println
  }

}
