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
      private var currentLine = rdr.readLine()
      override def hasNext = currentLine == null

      override def next() = {
        val res = currentLine
        currentLine = rdr.readLine()
        res
      }
    }
  }

  implicit object ReaderReadable extends Readable[Reader] {
    def readLines(rdr:Reader):Iterator[String] = Readable.readLines(new BufferedReader(rdr))
  }

  implicit object ISReadable extends Readable[InputStream] {
    def readLines(is:InputStream):Iterator[String] = Readable.readLines(new InputStreamReader(is))
  }

  implicit object FileReadable extends Readable[File] {
    def readLines(f:File):Iterator[String] = Readable.readLines(new FileReader(f))
  }

  implicit object FilenameReadable extends Readable[String] {
    def readLines(fn:String):Iterator[String] = Readable.readLines(new FileReader(fn))
  }

  implicit object URLReadable extends Readable[URL] {
    def readLines(url:URL):Iterator[String] = Readable.readLines(url.openStream())
  }

}

