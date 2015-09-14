package so.modernized

import java.io.File

import scala.collection.mutable
import scala.xml._

/**
 * Created by johnsullivan on 9/14/15.
 */
object UpdateLaunch {
  def main(args:Array[String]): Unit = {
    val lFile = new File(args(0))
    val launcherName = lFile.getName.split("""\.""").head
    val launcher = XML.loadFile(lFile)

    (launcher \\ "stringAttribute").find(n => (n \ "@key").toString() == "target_bundles").map { n =>
      (n \ "@value").toString().split(",")
    }

    val m = mutable.HashMap[String, AntValue[_]]()

    launcher.child.collect {
      case e@Elem(_, label, attr, _) if label.endsWith("Attribute") && attr.get("key").isDefined =>
        val key = attr("key").toString()
        val valueOpt = attr.get("value")

        label.reverse.drop("Attribute".length).reverse match {
          case "boolean" => m += key -> BooleanVal(valueOpt.get.toString().toBoolean)
          case "string" => m += key -> StringVal(valueOpt.get.toString())
          case "int" =>  m += key -> IntVal(valueOpt.get.toString().toInt)
          case "list" =>
            val values = e.child.collect {
              case Elem(_, "listEntry", lAttr, _) => lAttr("value").toString()
            }.toList
            m += key -> ListVal(values)
        }
    }

    m foreach println

    /*
    n match {
      case e@Elem(_, label, attr, _) if label.endsWith("Attribute") && attr.get("key").isDefined =>
        val key = attr("key").toString()
        val valueOpt = attr.get("value")
         m += (label.drop("Attribute".length) match {
          case "boolean" => key -> BooleanVal(valueOpt.get.toString().toBoolean)
          case "string" => key -> StringVal(valueOpt.get.toString())
          case "int" => key -> IntVal(valueOpt.get.toString().toInt)
          case "list" =>
            val values = e.child.collect {
              case Elem(_, "listEntry", lAttr, _) => lAttr("value").toString()
            }.toList
            key -> ListVal(values)
        })
    }
    */

    val idea = <component name="ProjectRunConfigurationManager">
      <configuration default="false" name={launcherName} type="#org.osmorc.OsgiConfigurationType" factoryName="OSGi Bundles" vmParameters="" programParameters="" includeAllBundlesInClassPath="false" workingDir="$PROJECT_DIR$/out/run.osgi/" useAlternativeJre="false" alternativeJrePath="" frameworkStartLevel="1" defaultStartLevel="5" generateWorkingDir="false">

        <framework instance="Equinox (3.10.2.v20150203-1939)" />
        <additinalProperties startConsole="true" equinoxApplication="" systemPackages="" equinoxProduct="" debugMode="false" bootDelegation="" />
        <method />
      </configuration>
    </component>
  }
}


sealed trait AntValue[A] { def value:A }

case class BooleanVal(value:Boolean) extends AntValue[Boolean]
case class StringVal(value:String) extends AntValue[String]
case class IntVal(value:Int) extends AntValue[Int]
case class ListVal(value:List[String]) extends AntValue[List[String]]

object AntValue {

}
