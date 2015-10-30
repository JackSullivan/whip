package so.modernized.and.useful

import java.io._
import java.net.URL

import scala.util.{Failure, Success, Try}

trait Readable[A, B] extends (A => B)

@deprecated("mostly superceded by cc.factorie.itil.ISAble", "10/30/2015")
object Readable {
  def read[A, B](a:A)(implicit ev:Readable[A, B]):B = ev(a)
  def slurp[A](a:A)(implicit ev:Readable[A, Iterator[String]]) = ev(a).mkString("\n")

  def splitDelim[A](a:A, delim:String)(implicit ev:Readable[A, Iterator[String]]) = ev(a).map(_.split(delim))
  def splitTsv[A](a:A)(implicit ev:Readable[A, Iterator[String]]) = splitDelim(a, "\t")
  def splitCsv[A](a:A)(implicit ev:Readable[A, Iterator[String]]) = splitDelim(a, ",")
  def splitWhitespace[A](a:A)(implicit ev:Readable[A, Iterator[String]]) = splitDelim(a, """\s+""")

  implicit object BufferedReaderIteratorReadable extends Readable[BufferedReader, Iterator[String]] {
    def apply(r:BufferedReader):Iterator[String] =
      new Iterator[String] {
        private var nextLine = r.readLine()

        def hasNext = {
          val res = nextLine != null
          if(!res) r.close()
          res
        }

        def next() = {
          val res = nextLine
          nextLine = r.readLine()
          res
        }
      }
  }

  implicit object FileIteratorReadable extends Readable[File, Iterator[String]] {
    def apply(f:File):Iterator[String] = read(new BufferedReader(new FileReader(f)))
  }

  implicit object ReaderReadable extends Readable[Reader, Iterator[String]] {
    def apply(r:Reader):Iterator[String] = read(new BufferedReader(r))
  }

  implicit object InputStreamReadable extends Readable[InputStream, Iterator[String]] {
    def apply(is:InputStream):Iterator[String] = ReaderReadable(new BufferedReader(new InputStreamReader(is)))
  }

  implicit object URLReadable extends Readable[URL, Iterator[String]] {
    def apply(url:URL):Iterator[String] = InputStreamReadable(url.openStream())
  }

  implicit object FloatDataReadable extends Readable[DataInputStream, Iterator[Float]] {
    def apply(ds:DataInputStream):Iterator[Float] =
      new Iterator[Float] {
        var nextFloat = Try(ds.readFloat())
        def hasNext = {
          nextFloat match {
            case Success(_) => true
            case Failure(_) => ds.close(); false
          }
        }

        def next() = {
          val Success(res) = nextFloat
          nextFloat = Try(ds.readFloat())
          res
        }
      }
  }

  implicit object FileFloatReadable extends Readable[File, Iterator[Float]] {
    def apply(f:File):Iterator[Float] = read(new DataInputStream(new FileInputStream(f)))
  }

  implicit object InputStreamFloatReadable extends Readable[InputStream, Iterator[Float]] {
    def apply(is:InputStream):Iterator[Float] = read(new DataInputStream(is))
  }
}
