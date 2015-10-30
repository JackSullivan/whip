package so.modernized.whip.util

import java.io.{File, FileFilter}

trait FileLike[A] {
  def getRecursively(a:A, pred:File => Boolean):Seq[File]
  final def getRecursively(a:A):Seq[File] = getRecursively(a, always)
}

object FileLike {
  implicit object FileFileLike extends FileLike[File] {
    def getRecursively(dir:File, pred:File => Boolean):Seq[File] = if (dir.isDirectory) {
      println("found dir "+ dir.getAbsolutePath)
      dir.listFiles(new FileFilter {
        override def accept(pathname: File): Boolean = {
          println(pathname.getAbsolutePath + "\t" + pred(pathname))
          pred(pathname)
        }
      }).flatMap(getRecursively(_, pred))
    } else {
      (if(pred(dir)) Some(dir) else None).toSeq
    }
  }

  implicit object StringFileLike extends FileLike[String] {
    def getRecursively(s:String, pred:File => Boolean) = FileFileLike.getRecursively(new File(s), pred)
  }

  def getAllIn[A](a:A)(implicit ev:FileLike[A]) = ev.getRecursively(a)
  def getInWhere[A](a:A, pred:File => Boolean)(implicit ev:FileLike[A]) = ev.getRecursively(a, pred)
}

