package com.cambridgesemantics.anzo.unstructured.graphsummarization

trait Vectorable {

}

object VectorOps {
  implicit class Vector(val xs:Array[Double]) extends AnyVal {

    def dot(ys:Array[Double]):Double = {
      assert(xs.length == ys.length)
      var res = 0.0
      var idx = 0
      while(idx < xs.length) {
        res += xs(idx) * ys(idx)
        idx += 1
      }
      res
    }
  }
}
