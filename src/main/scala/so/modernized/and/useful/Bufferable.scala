package so.modernized.and.useful
import java.nio.{DoubleBuffer, IntBuffer, FloatBuffer, Buffer}

trait Bufferable[A, B <: Buffer] extends (A => B)


object Bufferable {
  implicit object FloatArrayBuffer extends Bufferable[Array[Float], FloatBuffer] {
    def apply(a: Array[Float]): FloatBuffer = FloatBuffer wrap a
  }

  implicit object IntArrayBuffer extends Bufferable[Array[Int], IntBuffer] {
    def apply(a:Array[Int]): IntBuffer = IntBuffer wrap a
  }

  implicit object DoubleArrayBuffer extends Bufferable[Array[Double], DoubleBuffer] {
    def apply(a:Array[Double]): DoubleBuffer = DoubleBuffer wrap a
  }

  implicit def wrapBuffer[A, B](a:A)(implicit ev:Bufferable[A, B]) = ev(a)
}
