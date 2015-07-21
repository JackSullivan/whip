package so.modernized.whip

import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.functions._

import scala.reflect.ClassTag

/**
 * Created by johnsullivan on 7/17/15.
 */
class Foo[A](implicit val ct:) {
  val fn = udf[A, A, A, Boolean]({(x:A, y:A, pred:Boolean) => if (pred) x else y})
  val afn = udf({(x:Any, y:Any, pred:Boolean) => if (pred) x else y})
}

object Bar {
  def main(args:Array[String]): Unit = {
    val df:DataFrame = null
    ClassTag(Class.forName(df.schema("name").dataType.typeName))
    df.withColumn("result", new Foo[] (df("col1"), df("col2"), df("pred")))
  }
}
