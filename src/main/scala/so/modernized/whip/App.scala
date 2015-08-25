package so.modernized.whip

import org.openanzo.glitter.query.PatternSolution
import org.openanzo.rdf.URI

import scala.io.Source

/**
 * @author ${user.name}
 */

/*
object App {

  def main(args : Array[String]): Unit = {
    println(LoadPlaces("Places-cut.tsv").take(10).toList.mkString("\n"))
  }

}

object LoadPlaces {
  def apply(file:String) = Source.fromFile(file).getLines().map { line =>
    val arr = line split "\t"
    println(arr.toList)
    val Array(name, altNames1, altNames2, pop, lat, lon) = line.split("\t")
    val names = (Seq(name) ++ altNames1.split(",") ++ altNames2.split(",")).map(_.trim).toSet
    val population = if(pop.isEmpty) None else Some(pop.toInt)
    val coords = if(lat.nonEmpty && lon.nonEmpty) Some(Coords(lat.toDouble, lon.toDouble)) else None

    Place(names, population, coords)
  }
}

case class Coords(lat:Double, lon:Double)
case class Place(names:Set[String], population:Option[Int], coords:Option[Coords])

object Foo {
  import sparql._
  val ps:PatternSolution = null
  case class Bar(name:String, loc:URI)

  ps.extract(r => Bar(r[String], r[URI]))
}
*/
