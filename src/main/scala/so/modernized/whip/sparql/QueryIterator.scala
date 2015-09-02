package so.modernized.whip.sparql

import org.openanzo.client.IAnzoClient
import org.openanzo.glitter.query.PatternSolution
import org.openanzo.rdf.{URI => AnzoURI}

import scala.collection.JavaConverters._

class QueryIterator(anzo:IAnzoClient, datasets:Set[AnzoURI])(query:(Int, Int) => String, batchSize:Int=1000) extends Iterator[Seq[PatternSolution]] {
  private var cursor = 0
  private var lastReturnedRows = 0
  override def hasNext: Boolean = lastReturnedRows == batchSize

  override def next(): Seq[PatternSolution] = {
    val q = query(cursor, batchSize)
    println(q)
    val res = anzo.serverQuery(null, null, datasets.asJava, q).getSelectResults.asScala
    cursor += batchSize
    lastReturnedRows = res.size
    res
  }
}
