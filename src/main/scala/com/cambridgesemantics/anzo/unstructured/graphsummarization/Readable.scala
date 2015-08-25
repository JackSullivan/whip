package com.cambridgesemantics.anzo.unstructured.graphsummarization

import java.io._
import java.net.URL

trait Readable[A] {
  def readLines(a:A):Iterator[String]
  def slurp(a:A) = readLines(a).mkString("\n")
}

object Readable {
  def readLines[A : Readable](a:A) = implicitly[Readable[A]].readLines(a)
  def slurp[A : Readable](a:A) = implicitly[Readable[A]].slurp(a)

  implicit object BuffReadable extends Readable[BufferedReader] {
    def readLines(rdr:BufferedReader) = new Iterator[String] {
      private var nextLine = rdr.readLine()

      def next() = {
        val res = nextLine
        nextLine = rdr.readLine()
        if(nextLine == null) {
          rdr.close()
        }
        res
      }

      def hasNext = nextLine != null
    }
  }

  implicit object ReaderReadable extends Readable[Reader] {
    def readLines(rdr:Reader):Iterator[String] = Readable.readLines(new BufferedReader(rdr))
  }

  implicit object ISReadable extends Readable[InputStream] {
    def readLines(is:InputStream):Iterator[String] = Readable.readLines(new InputStreamReader(is).asInstanceOf[Reader])
  }

  implicit object FileReadable extends Readable[File] {
    def readLines(f:File):Iterator[String] = Readable.readLines(new FileReader(f).asInstanceOf[Reader])
  }

  implicit object FilenameReadable extends Readable[String] {
    def readLines(fn:String):Iterator[String] = Readable.readLines(new FileReader(fn).asInstanceOf[Reader])
  }

  implicit object URLReadable extends Readable[URL] {
    def readLines(url:URL):Iterator[String] = Readable.readLines(url.openStream())
  }

}

