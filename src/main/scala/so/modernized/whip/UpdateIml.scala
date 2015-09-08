package so.modernized.whip

import java.io.{FileFilter, File}

import org.apache.commons.io.FileUtils
import scala.xml.{Node, Elem, NodeSeq, XML}

object UpdateIml {

  def getDirs(dir:File, pred:File =>Boolean):Seq[File] = if(pred(dir)) {
    Seq(dir)
  } else {
    dir.listFiles().filter(pred).flatMap(getDirs(_, pred))
  }

  def main(args:Array[String]): Unit = {
    val rootDir = args(0)



    getDirs(new File(rootDir), {f => f.isDirectory && f.listFiles.exists(_.getName.endsWith(".iml"))}).foreach { imlDir =>
      println("found iml dir " + imlDir.getAbsolutePath)
      val imlFile = imlDir.listFiles().find(_.getName.endsWith(".iml")).get
      println("found iml file " + imlFile.getAbsolutePath)
      val dirs = imlDir.listFiles(new FileFilter {
        override def accept(pathname: File): Boolean = pathname.isDirectory
      }).toSet
      FileUtils.copyFile(imlFile, new File(imlFile.getAbsolutePath + ".bak"))
      val xml = XML loadFile imlFile


      val contentNodes = NodeSeq.fromSeq(Seq(
        dirs.find(_.getName == "src").map(_ => <sourceFolder url="file://$MODULE_DIR$/src" isTestSource="false" />),
        dirs.find(_.getName == "gensrc").map(_ => <sourceFolder url="file://$MODULE_DIR$/gensrc" isTestSource="false" generated="true" />),
        dirs.find(_.getName == "bin").map(_ => <excludeFolder url="file://$MODULE_DIR$/bin" />),
        dirs.find(_.getName == "target").map(_ => <excludeFolder url="file://$MODULE_DIR$/target" />)).flatten)

      println("generating content nodes: ")
      contentNodes foreach println

      val libXml = <orderEntry type="module-library">
        <library name="lib">
          <CLASSES>
            <root url="file://$MODULE_DIR$/lib" />
          </CLASSES>
          <JAVADOC />
          <SOURCES>
            <root url="file://$MODULE_DIR$/lib" />
          </SOURCES>
          <jarDirectory url="file://$MODULE_DIR$/lib" recursive="false" />
          <jarDirectory url="file://$MODULE_DIR$/lib" recursive="false" type="SOURCES" />
        </library>
      </orderEntry>

      val libModule =
        dirs.find(_.getName == "lib").map(_ => libXml)

      if(libModule.isDefined) println("generating libModule")

      def traverse(e:Node):Node = {
        //println(e.label, e.attributes)
        e match {
          case <content>{cs @ _* }</content> =>
            println("content")
            println(cs)
            <content url="file://$MODULE_DIR$">{contentNodes}</content>
          case n if n.label == "orderEntry" && (n \ "@type").toString() == "module-library" =>
            println("orderEntry")
            println(n)
            <orderEntry type="module-library">{ n.child ++ libModule}</orderEntry>
          case node if node.child.nonEmpty =>
            //println("recurse")
            Elem(node.prefix, node.label, node.attributes, node.scope, true, node.child.map(traverse):_*)
          case leaf =>
            //println("leaf")
            leaf

        }
      }

      val newXml = traverse(xml)
      //println(newXml)

      XML.save("res.iml", newXml)
    }
  }

}
