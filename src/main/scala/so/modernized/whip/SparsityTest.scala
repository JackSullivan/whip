package so.modernized.whip

import cc.factorie.app.nlp.lexicon.StaticLexicons
import cc.factorie.app.nlp.ner.ConllChainNer
import cc.factorie.la.Tensor
import cc.factorie.maths

/**
 * @author johnsullivan
 */
/*
object SparsityTest {

  def nonEmptyProp(t:Tensor): Double = {
    var nonEmpties = 0
    t.foreachActiveElement{ case(i, v) =>
      if(!maths.almostEquals(v, 0, 0.0001)) {
        nonEmpties += 1
      }
    }
    nonEmpties.toDouble / t.size
  }


  def main(args:Array[String]): Unit = {
    SpeedTestCmdOpts parse args
    import SpeedTestCmdOpts._

    val ner = new ConllChainNer()(nerModel.value, new StaticLexicons()(lexiconFile.value))


    ner.model.parameters.tensors.foreach(t => println(t.getClass.getName, t.size, nonEmptyProp(t)))
  }

}
*/
