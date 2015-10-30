package so.modernized.whip.testing

import java.io.{BufferedWriter, File, FileWriter}
import java.net.URL

import cc.factorie.app.nlp.lexicon.{LexiconsProvider, StaticLexicons}
import cc.factorie.app.nlp.load.{BILOUChunkDomain, BILOUChunkTag, LoadConll2003}
import cc.factorie.app.nlp.ner._
import cc.factorie.app.nlp.segment.{DeterministicSentenceSegmenter, DeterministicTokenizer}
import cc.factorie.app.nlp.{Document => FacDocument, DocumentAnnotator, Sentence, Token}
import cc.factorie.util.{CmdOptions, DefaultCmdOptions, ModelProvider, ModelProviderCmdOptions}
import gate.creole.{ANNIEConstants, SerialAnalyserController}
import gate.{Annotation, Factory => GateFactory, Gate, ProcessingResource}

import scala.collection.JavaConverters._
import scala.collection.mutable

/**
 * @author johnsullivan
 */
object Evaluation {
  /* Compute segment- and chunk type-level F scores (as well as overall) */
  def eval(sentences: Seq[Sentence]) = {
    // get number of gold chunks of each length, type; number of correct chunks of each length, type
    val goldCounts = collection.mutable.HashMap[(Int, String),Int]()
    val predictedCounts = collection.mutable.HashMap[(Int, String),Int]()
    val correctCounts = collection.mutable.HashMap[(Int, String),Int]()
    var malformedCount = 0

    val chunkTypes = BILOUChunkDomain.categories.map { c => if (c == "O") "O" else c.drop(2) }.distinct.sorted
    val bilous = Array('B', 'I', 'L', 'U', 'O')

    // order: expected, actual
    val typeConfusion = Array.fill(chunkTypes.length)(Array.fill(chunkTypes.length)(0))
    val tagConfusion = Array.fill(BILOUChunkDomain.size)(Array.fill(BILOUChunkDomain.size)(0))
    val bilouConfusion = Array.fill(bilous.size)(Array.fill(bilous.size)(0))

    def addChunk(chunkType: String, chunkLen: Int, correct: Boolean) = {
      if(correct){
        if(!correctCounts.contains((chunkLen, chunkType))) correctCounts((chunkLen, chunkType)) = 0
        correctCounts((chunkLen, chunkType)) += 1
      }
      if(!predictedCounts.contains((chunkLen, chunkType))) predictedCounts((chunkLen, chunkType)) = 0
      predictedCounts((chunkLen, chunkType)) += 1
    }

    def addGoldChunk(chunkType: String, chunkLen: Int) = {
      if(!goldCounts.contains((chunkLen, chunkType))) goldCounts((chunkLen, chunkType)) = 0
      goldCounts((chunkLen, chunkType)) += 1
    }

    sentences.foreach{ sentence =>
      var currentChunkLen = 0
      var currentChunkType = ""
      var currentGoldChunkLen = 0
      var currentGoldChunkType = ""
      val iter = sentence.tokens.iterator
      var lastTag = "O"
      var lastGoldTag = "O"
      var currentChunkIsCorrect = true
      while(iter.hasNext){
        val tok = iter.next()
        val tag = tok.attr[BILOUChunkTag]
        val tagType = if(tag.categoryValue == "O") "O" else tag.categoryValue.drop(2)
        val goldTagType = if(tag.target.categoryValue == "O") "O" else tag.target.categoryValue.drop(2)
        val bilou = tag.categoryValue.head
        val goldBilou = tag.target.categoryValue.head
        // compute all the confusion stuff
        if(!tag.valueIsTarget) tagConfusion(tag.intValue)(tag.target.intValue) += 1
        if(tagType != goldTagType) typeConfusion(chunkTypes.indexOf(goldTagType))(chunkTypes.indexOf(tagType)) += 1
        if(bilou != goldBilou) bilouConfusion(bilous.indexOf(goldBilou))(bilous.indexOf(bilou)) += 1
        bilou match {
          case 'B' =>
            // need to add new label containing buffer to first tag in buffer; reset buffer
            if(currentChunkLen != 0) {
              if(lastTag(0) != 'L' && lastTag(0) != 'U' && lastTag(0) != 'O') currentChunkIsCorrect = false // catches cases when we correctly predicted part but not the whole thing
              addChunk(currentChunkType, currentChunkLen, currentChunkIsCorrect)
              currentChunkLen = 0
              currentChunkIsCorrect = true
            }
            currentChunkIsCorrect &= tag.valueIsTarget
            currentChunkType = tagType
            currentChunkLen += 1
          case 'I' =>
            if(lastTag(0) != 'I' && lastTag(0) != 'B'){
              malformedCount += 1
              if(currentChunkLen != 0) {
                addChunk(currentChunkType, currentChunkLen, currentChunkIsCorrect)
                currentChunkLen = 0
              }
              currentChunkIsCorrect = false
            }
            currentChunkType = tagType
            currentChunkIsCorrect &= tag.valueIsTarget
            currentChunkLen += 1
          case 'L' =>
            if(lastTag(0) != 'I' && lastTag(0) != 'B') {
              malformedCount += 1
              if (currentChunkLen != 0) {
                addChunk(currentChunkType, currentChunkLen, currentChunkIsCorrect)
                currentChunkLen = 0
              }
              currentChunkIsCorrect = false
            }
            currentChunkType = tagType
            currentChunkIsCorrect &= tag.valueIsTarget
            currentChunkLen += 1
            addChunk(currentChunkType, currentChunkLen, currentChunkIsCorrect)
            currentChunkLen = 0
            currentChunkIsCorrect = true
          case 'U' =>
            if(currentChunkLen != 0) {
              if(lastTag(0) != 'L' && lastTag(0) != 'U' && lastTag(0) != 'O'){
                currentChunkIsCorrect = false
                malformedCount += 1
              } // catches cases when we correctly predicted part but not the whole thing
              addChunk(currentChunkType, currentChunkLen, currentChunkIsCorrect)
              currentChunkLen = 0
            }
            currentChunkType = tagType
            currentChunkIsCorrect = tag.valueIsTarget
            addChunk(currentChunkType, 1, currentChunkIsCorrect)
            currentChunkIsCorrect = true
          case 'O' =>
            if(currentChunkLen != 0 && lastTag(0) != 'O'){
              if(lastTag(0) != 'L' && lastTag(0) != 'U'){
                currentChunkIsCorrect = false
                malformedCount += 1
              } // catches cases when we correctly predicted part but not the whole thing
              addChunk(currentChunkType, currentChunkLen, currentChunkIsCorrect)
              currentChunkLen = 0
              currentChunkIsCorrect = true
            }
            currentChunkType = tagType
            currentChunkIsCorrect &= tag.valueIsTarget
            currentChunkLen += 1
        }

        goldBilou match {
          case 'B' =>
            // need to add new label containing buffer to first tag in buffer; reset buffer
            if(currentGoldChunkLen != 0){
              addGoldChunk(currentGoldChunkType, currentGoldChunkLen)
              currentGoldChunkLen = 0
            }
            currentGoldChunkType = goldTagType
            currentGoldChunkLen += 1
          case 'I' =>
            currentGoldChunkLen += 1
          case 'L' =>
            currentGoldChunkLen += 1
            addGoldChunk(currentGoldChunkType, currentGoldChunkLen)
            currentGoldChunkLen = 0
          case 'U' =>
            if(currentGoldChunkLen != 0) {
              addGoldChunk(currentGoldChunkType, currentGoldChunkLen)
              currentGoldChunkLen = 0
            }
            currentGoldChunkType = goldTagType
            addGoldChunk(currentGoldChunkType, 1)
          case 'O' =>
            if(currentGoldChunkLen != 0 && lastGoldTag(0) != 'O'){
              addGoldChunk(currentGoldChunkType, currentGoldChunkLen)
              currentGoldChunkLen = 0
            }
            currentGoldChunkType = goldTagType
            currentGoldChunkLen += 1
        }
        lastTag = tag.categoryValue
        lastGoldTag = tag.target.categoryValue

      }
      // don't skip last one
      if(currentChunkLen != 0) {
        addChunk(currentChunkType, currentChunkLen, currentChunkIsCorrect)
      }
      if(currentGoldChunkLen != 0) addGoldChunk(currentGoldChunkType, currentGoldChunkLen)
    }

    println(s"malformed chunks: $malformedCount")

    println("TYPE CONFUSION: ")
    println("\t" + chunkTypes.mkString("\t"))
    typeConfusion.zipWithIndex.foreach{case(goldType, idx) => println(chunkTypes(idx) + "\t" + goldType.mkString("\t"))}

    println("TAG CONFUSION: ")
    println("\t" + BILOUChunkDomain.categories.mkString("\t"))
    tagConfusion.zipWithIndex.foreach{case(goldTag, idx) => println(BILOUChunkDomain.category(idx) + "\t" + goldTag.mkString("\t"))}

    println("BILOU CONFUSION: ")
    println("\t" + bilous.mkString("\t"))
    bilouConfusion.zipWithIndex.foreach{case(goldBilou, idx) => println(bilous(idx) + "\t" + goldBilou.mkString("\t"))}

    // now aggregate counts over length and type
    println("GOLD DISTRIBUTION")

    println("\t" + chunkTypes.mkString("\t") + "\tTOT")

    val goldChunkLengths = goldCounts.keys.map(_._1).toSet.toSeq.sorted
    chunkTypes.foreach{chunkType => goldChunkLengths.foreach{len => if(!goldCounts.contains((len, chunkType))) goldCounts((len, chunkType)) = 0}}
    val goldCountsPerLen = for(length <- goldChunkLengths) yield {
      val counts = goldCounts.filterKeys(_._1 == length).toSeq.sortBy(_._1._2)
      val total = counts.filterNot(_._1._2 == "O").map(_._2).sum
      println(length + "\t" + counts.map(g => g._2).mkString("\t") + "\t" + total)
      total
    }
    val goldTypeCounts = goldCounts.groupBy(_._1._2).map{case(k, m) => (k, m.values.sum)}.toSeq.sortBy(_._1)
    val totalGold = goldTypeCounts.filterNot(_._1 == "O").map(_._2).sum
    println("TOT\t" + goldTypeCounts.map(_._2).mkString("\t") + "\t" + totalGold)
    assert(totalGold == goldCountsPerLen.sum)


    println("PREDICTED (ALL) DISTRIBUTION")
    println("\t" + chunkTypes.mkString("\t") + "\tTOT")
    val predictedChunkLengths = predictedCounts.keys.map(_._1).toSet.toSeq.sorted
    chunkTypes.foreach{chunkType => predictedChunkLengths.foreach{len => if(!predictedCounts.contains((len, chunkType))) predictedCounts((len, chunkType)) = 0}}
    val predictedCountsPerLen = for(length <- predictedChunkLengths) yield {
      val counts = predictedCounts.filterKeys(_._1 == length).toSeq.sortBy(_._1._2)
      val total = counts.filterNot(_._1._2 == "O").map(_._2).sum
      //      println(counts.map(_._1._2).toSet.mkString(" "))
      println(length + "\t" + counts.map(g => g._2).mkString("\t") + "\t" + total)
      total
    }
    val predictedTypeCounts = predictedCounts.groupBy(_._1._2).map{case(k, m) => (k, m.values.sum)}.toSeq.sortBy(_._1)
    val totalPredicted = predictedTypeCounts.filterNot(_._1 == "O").map(_._2).sum
    println("TOT\t" + predictedTypeCounts.map(_._2).mkString("\t") + "\t" + totalPredicted)
    assert(totalPredicted == predictedCountsPerLen.sum)

    // if correct=0:
    // recall=1.0, since we found all 0 true positives
    // precision=0.0 if there are any false positives (predicted-correct) and 1.0 otherwise

    println("PREDICTED (CORRECT) DISTRIBUTION")
    println("\t" + chunkTypes.mkString("\t") + "\tTOT\tP\tR\tF")
    val correctChunkLengths = correctCounts.keys.map(_._1).toSet.toSeq.sorted
    chunkTypes.foreach{chunkType => correctChunkLengths.foreach{len => if(!correctCounts.contains((len, chunkType))) correctCounts((len, chunkType)) = 0}}
    val correctCountsPerLen = for((length, idx) <- correctChunkLengths.zipWithIndex) yield {
      val counts = correctCounts.filterKeys(_._1 == length).toSeq.sortBy(_._1._2)
      val total = counts.filterNot(_._1._2 == "O").map(_._2).sum
      val precision = if(predictedCountsPerLen(idx) == 0 && total == 0) 1.0 else total.toDouble/predictedCountsPerLen(idx)
      val recall = if(goldCountsPerLen(idx) == 0 && total == 0) 1.0 else if(goldCountsPerLen(idx) == 0) 0.0 else total.toDouble/goldCountsPerLen(idx)
      val f = if(precision+recall == 0) 0.0 else 2*precision*recall/(precision+recall)
      println(length + "\t" + counts.map(g => g._2).mkString("\t") + "\t" + total + "\t" + f"${precision*100}%2.2f" + "\t" + f"${recall*100}%2.2f" + "\t" + f"${f*100}%2.2f")
      total
    }
    val correctTypeCounts = correctCounts.groupBy(_._1._2).map{case(k, m) => (k, m.values.sum)}.toSeq.sortBy(_._1)
    val typePrecisions = correctTypeCounts.zip(predictedTypeCounts).map{case(correct, pred) => if(pred._2 == 0 && correct._2 == 0) 1.0 else correct._2.toDouble/pred._2}
    val typeRecalls = correctTypeCounts.zip(goldTypeCounts).map{case(correct, gold) => if(gold._2 == 0 && correct._2 == 0) 1.0 else if(gold._2 == 0) 0.0 else correct._2.toDouble/gold._2}
    val fScores = typePrecisions.zip(typeRecalls).map{case(p, r) => if(p+r == 0) 0.0 else 2*p*r/(p+r)}
    val totalCorrect = correctTypeCounts.filterNot(_._1 == "O").map(_._2).sum
    val totalPrecision = totalCorrect.toDouble/totalPredicted
    val totalRecall = totalCorrect.toDouble/totalGold
    val totalF = 2*totalPrecision*totalRecall/(totalPrecision+totalRecall)
    println("TOT\t" + correctTypeCounts.map(_._2).mkString("\t")+ "\t" + totalCorrect)
    assert(totalCorrect == correctCountsPerLen.sum)
    println("P\t" + typePrecisions.map{p => f"${p*100}%2.2f"}.mkString("\t") + "\t" + f"${totalPrecision*100}%2.2f")
    println("R\t" + typeRecalls.map{r => f"${r*100}%2.2f"}.mkString("\t") + "\t" + f"${totalRecall*100}%2.2f")
    println("F\t" + fScores.map{f => f"${f*100}%2.2f"}.mkString("\t") + "\t" + f"${totalF*100}%2.2f")

  }
}

case class Evaluation(tp:Double, fp:Double, fn:Double) {
  def + (that:Evaluation) = Evaluation(this.tp + that.tp, this.fp + that.fp, this.fn + that.fn)
  def prec = tp.toDouble / (tp + fp)
  def rec = tp.toDouble / (tp + fn)
  def f1 = 2.0 * ((prec * rec) / (prec + rec))

  def show =
    "PRECSION: %.3f\n".format(prec) +
      "RECALL  : %.3f\n".format(rec) +
      "F1      : %.3f\n".format(f1)
}

case class SimpleToken(s:String, start:gate.Node, end:gate.Node) {
  var label = "O"
  var target = "O"

  def show = s"$s\t$label\t$target"
  def matches = Runagate.baseLabel(label) == Runagate.baseLabel(target)
}

case class Chunk(s:String, start:Int, end:Int, label:String) {
  lazy val key = start -> end
  var used = false
}

class EvalPart(var tp:Int, var fp:Int, var fn:Int) {
  def +(that:EvalPart) = new EvalPart(this.tp + that.tp, this.fp + that.fp, this.fn + that.fn)
  def prec = tp.toDouble / (tp + fp)
  def rec = tp.toDouble / (tp + fn)
  def f1 = 2.0 * ((prec * rec) / (prec + rec))

  def show =
    "PRECSION: %.5f\n".format(prec) +
      "RECALL  : %.5f\n".format(rec) +
      "F1      : %.5f\n".format(f1)
  override def toString = s"EvalPart($tp, $fp, $fn)"
}

object EvalPart {
  def empty = new EvalPart(0,0,0)
  def unapplySeq(ep:EvalPart) = Some((ep.tp, ep.fp, ep.fn))
}

object Runagate {

  implicit class EvalMapExtras(val m1:Map[String, EvalPart]) extends AnyVal{
    def +(m2:Map[String, EvalPart]) = (m1.keySet | m2.keySet).map {k =>
      k -> (m1(k) + m2(k))
    }.toMap
  }

  def baseLabel(s:String) = if(s == "O") s else s.split("-")(1)

  val docStrings = List("Jack and Jill went up the Hill to fetch a pail of water. They got sued by Time Warner and litigation is ongoing")

  def factorie2Gate(facDoc:FacDocument):gate.Document = {
    val params = GateFactory.newFeatureMap()

    Seq (
      gate.Document.DOCUMENT_STRING_CONTENT_PARAMETER_NAME -> facDoc.tokens.map(_.string).mkString(" "),
      gate.Document.DOCUMENT_MIME_TYPE_PARAMETER_NAME -> "text/html",
      "preserveOriginalContent" -> new java.lang.Boolean(true),
      "collectRepositioningInfo" -> new java.lang.Boolean(true)
    ).foreach{case (k,v) => params.put(k,v)}
    val gDoc = GateFactory.createResource("gate.corpora.DocumentImpl", params).asInstanceOf[gate.Document]
    gDoc.setSourceUrl(new URL("file://" + facDoc.name))
    gDoc.setName(facDoc.name)
    gDoc
  }

  implicit object AnnoOrdering extends Ordering[gate.Annotation] {
    override def compare(x: Annotation, y: Annotation): Int = x compareTo y
  }

  val annieAnnos = Seq("Address", "Date", "JobTitle", "Location", "Money", "Organization", "Person", "JobId", "Date_Posted")
  val annoMap = Map("Person" -> "PER", "Location" -> "LOC", "Address" -> "MISC", "Organization" -> "ORG", "Date" -> "MISC", "JobTitle" -> "MISC")

  def getTokens(anSet:gate.AnnotationSet):Seq[String] = anSet.asScala.toSeq.filter(_.getType == "Token").sorted.map(_.getFeatures.get("string").toString)

  def evaluateDocChunk(gDoc:gate.Document, fDoc:FacDocument):Map[String, EvalPart] = {

    val trueChunks = fDoc.attr[ConllNerSpanBuffer]
      .map(span => Chunk(span.string, span.head.stringStart, span.last.stringEnd, span.label.categoryValue)).toSeq
      //.map(c => c.key -> c).toMap

    val annos = gDoc.getAnnotations

    val predictedChunks = annos.asScala.filter(a => annoMap.contains(a.getType)).toSeq.sorted
      .map{ a =>
      val str = annos.get(a.getStartNode.getOffset, a.getEndNode.getOffset).asScala
        .filter(_.getType == "Token").toSeq.sorted
        .map(_.getFeatures.get("string").toString).mkString(" ")
      Chunk(str, a.getStartNode.getOffset.toInt + 1, a.getEndNode.getOffset.toInt + 1, annoMap(a.getType))
    }//.map(c => c.key -> c).toMap

    //trueChunks.take(10).zip(predictedChunks.take(10)).foreach {case(t,p) => println(t + "\t" + p)}

    val scoreMap = new mutable.HashMap[String, EvalPart]
    scoreMap ++= Seq("ORG", "LOC", "PER", "MISC").map(_ -> EvalPart.empty)

    var tp = 0
    var fp = 0
    var fn = 0

    val predMap = new mutable.HashMap[(Int, Int), Chunk]
      predMap ++= predictedChunks.map(c => (c.start, c.end) -> c)

    trueChunks.sortBy(_.start).foreach { case Chunk(gString, gStart, gEnd, gLabel) =>
      predMap.get(gStart -> gEnd) match {
        case Some(Chunk(pString, _, _, pLabel)) =>
          if(gLabel == pLabel) {
            scoreMap(gLabel).tp += 1 // we get it right!
          } else {
            // otherwise we have a false positive for the prediction and a false negative for gold
            scoreMap(gLabel).fn += 1
            scoreMap(pLabel).fp += 1
          }
          // we've used this pred
          predMap.remove(gStart -> gEnd)
        case None => scoreMap(gLabel).fn += 1 // there was no predicted chunk for this gold chunk, so we have a false negative
      }
    }

    //once we've check all the true chunks, all the predicted chunks leftover are false positives
    predMap.values.foreach(c => scoreMap(c.label).fp += 1)
    scoreMap.toMap

    /*
    for(g@Chunk(gString, gStart, gEnd, gLabel) <- trueChunks;
        p@Chunk(pString, pStart, pEnd, pLabel) <- predictedChunks
        if(!g.used && !p.used)) {
      if(gStart == pStart && gEnd == pEnd) {
        g.used = true
        p.used = true
        if(gLabel == pLabel) {
          tp += 1
        } else
      }
    }
    */
    /*
    trueChunks.take(10) foreach println
    predictedChunks.take(10) foreach println
    */
  }

  def evaluateDoc(gDoc:gate.Document, fDoc:FacDocument): Evaluation = {
    val annos = gDoc.getAnnotations
    val tokens = getTokens(gDoc.getAnnotations)
    //println(gDoc.getName)
    //println(tokens.mkString(" "))

    //annos.asScala.toSeq.filter(_.getType == "Token").sorted.foreach(print)


    /*
    val labelsMap = annos.asScala.filter(a => annoMap.keySet.contains(a.getType)).take(20).map{ a =>
      a.getStartNode
      a.getStartNode.getId.intValue() -> (a.getType, a.getEndNode.getId)
      //println(getTokens(gDoc.getAnnotations.get(a.getStartNode.getOffset, a.getEndNode.getOffset)).mkString(" ") -> annoMap(a.getType))
      //println(gDoc.getAnnotations.get(a.getStartNode.getOffset, a.getEndNode.getOffset).asScala.map(_.toString).mkString(""))
    }.toMap
    */

    val toks = annos.asScala.toSeq.filter(_.getType == "Token").sorted
      .map {t => SimpleToken(t.getFeatures.get("string").toString, t.getStartNode, t.getEndNode)}

    val labelsMap = annos.asScala
      .filter(a => annoMap.keySet.contains(a.getType)).toSeq.sorted
      .flatMap { a =>
      val nerLabel = annoMap(a.getType)
      annos.get(a.getStartNode.getOffset, a.getEndNode.getOffset).asScala
        .toSeq.filter(_.getType == "Token").sorted match {
        case Seq(ann) => Seq(ann.getStartNode.getId.intValue() -> ("U-" + nerLabel))
        case anns => anns.zipWithIndex.map {
          case(a2, 0) => a2.getStartNode.getId.intValue() -> ("B-" + nerLabel)
          case(a2, idx) if idx == anns.length => a2.getStartNode.getId.intValue -> ("L-" + nerLabel)
          case(a2, _) => a2.getStartNode.getId.intValue -> ("I-" + nerLabel)
        }
      }
    }.toMap


    toks.foreach { t =>
      labelsMap.get(t.start.getId.intValue()) match {
        case Some(label) => t.label = label
        case None => ()
      }
    }

    toks.foreach { t =>
      fDoc.asSection.offsetSnapToTokens(t.start.getOffset.toInt, t.end.getOffset.toInt) match {
        case Some(span) =>
          t.target = span.head.attr[LabeledBilouConllNerTag].categoryValue
          //println(t.show)
        case None => println(s"Gate token: $t had no appropriate span")
      }
    }


    var tp = 0.0
    var fp = 0.0
    var fn = 0.0
    toks.foreach { t =>
      if(t.matches && t.label != "O") {
        tp += 1
      } else if(t.label == "O" && t.target != "O") {
        fn += 1
      } else if(t.target == "O" && t.label != "O") {
        fp += 1
      }
    }
    Evaluation(tp, fp, fn)
  }

  def main(args:Array[String]): Unit = {
    val loader = new LoadConll2003(true, true)
    val docs = loader.fromSource(io.Source.fromFile(args(0))).map(BilouConllNerChunkAnnotator.process)


    sys.props += "gate.plugins.home" -> "/Users/johnsullivan/git/anzo/anzo/com.cambridgesemantics.anzo.unstructured/OSGI-INF/Gate/gate-6.1-build3913-ALL/plugins"
    sys.props += "gate.site.config" -> "/Users/johnsullivan/git/anzo/anzo/com.cambridgesemantics.anzo.unstructured/OSGI-INF/Gate/gate-6.1-build3913-ALL/gate.xml"
    Gate.setGateHome(new File("/Users/johnsullivan/git/anzo/anzo/com.cambridgesemantics.anzo.unstructured/OSGI-INF/Gate/gate-6.1-build3913-ALL"))
    Gate.setPluginsHome(new File("/Users/johnsullivan/git/anzo/anzo/com.cambridgesemantics.anzo.unstructured/OSGI-INF/Gate/gate-6.1-build3913-ALL/plugins"))
    Gate.init()

    val gDocs = docs map factorie2Gate

    //Gate.getCreoleRegister.registerDirectories(Paths.get(sys.props("gate.plugins.home")).toUri.toURL)
    Gate.getCreoleRegister.registerDirectories(new File("/Users/johnsullivan/git/anzo/anzo/com.cambridgesemantics.anzo.unstructured/OSGI-INF/Gate/gate-6.1-build3913-ALL/plugins", "ANNIE").toURI.toURL)

    val controller = GateFactory.createResource("gate.creole.SerialAnalyserController", GateFactory.newFeatureMap(), GateFactory.newFeatureMap(), "ANNIE_" + Gate.genSym()).asInstanceOf[SerialAnalyserController]
    controller.add(GateFactory.createResource("gate.creole.annotdelete.AnnotationDeletePR").asInstanceOf[ProcessingResource])
    ANNIEConstants.PR_NAMES.foreach { preReq =>
      controller.add(GateFactory.createResource(preReq, GateFactory.newFeatureMap()).asInstanceOf[ProcessingResource])
    }

    val corpus = GateFactory.newCorpus("gate.corpora.CorpusImpl")

    gDocs foreach corpus.asInstanceOf[java.util.List[gate.Document]].add
    controller setCorpus corpus
    controller.execute()

    docs.map { d =>
      d.tokens.toSeq.map(_.attr[LabeledBilouConllNerTag].target.value)
    }

    val scoreMap = controller.getCorpus.asInstanceOf[java.util.List[gate.Document]].asScala.zip(docs).map {case(gD, fD) => evaluateDocChunk(gD, fD)}.reduce(_ + _)

    scoreMap.foreach { case (key, ep) =>
      println(key)
      println(ep.show)
    }
    println("OVERALL")
    println(scoreMap.values.reduce(_ + _).show)

    println("OVERALL - MISC")
    println(scoreMap.filter(_._1 != "MISC").values.reduce(_ + _).show)
    //val score = controller.getCorpus.asInstanceOf[java.util.List[gate.Document]].asScala.zip(docs).map{case(gd, fd) => evaluateDoc(gd, fd)}.reduce(_ + _)
    //println(score.show)
  }
}

/*
object ConllBlankNerAnnotator extends NerAnnotator[ConllNerSpan, LabeledBilouConllNerTag] {
  def annotateTokens(document: FacDocument) = document
  def newBuffer = new ConllNerSpanBuffer
  def newSpan(sec: Section, start: Int, length: Int, category: String) = new ConllNerSpan(sec, start, length, category)
  def prereqAttrs = Seq(classOf[LabeledBilouConllNerTag])
}
*/

trait SpeedTestCmdOpts extends CmdOptions with ModelProviderCmdOptions with DefaultCmdOptions {
  val nerModel = new ModelCmdOption[ConllChainNer]
  val lexiconFile = new CmdOption[File]("lexicons", new File(""), "FILE", "")
  val conllData = new CmdOption[File]("conll-data", new File(""), "FILE", "")
}

object AnnieSpeedTest {
  val opts = new SpeedTestCmdOpts {}

  def main(args:Array[String]): Unit = {
    opts parse args
    import opts._

    val docs = new LoadConll2003(true, true).fromFile(conllData.value)

    sys.props += "gate.plugins.home" -> "/Users/johnsullivan/git/anzo/anzo/com.cambridgesemantics.anzo.unstructured/OSGI-INF/Gate/gate-6.1-build3913-ALL/plugins"
    sys.props += "gate.site.config" -> "/Users/johnsullivan/git/anzo/anzo/com.cambridgesemantics.anzo.unstructured/OSGI-INF/Gate/gate-6.1-build3913-ALL/gate.xml"
    Gate.setGateHome(new File("/Users/johnsullivan/git/anzo/anzo/com.cambridgesemantics.anzo.unstructured/OSGI-INF/Gate/gate-6.1-build3913-ALL"))
    Gate.setPluginsHome(new File("/Users/johnsullivan/git/anzo/anzo/com.cambridgesemantics.anzo.unstructured/OSGI-INF/Gate/gate-6.1-build3913-ALL/plugins"))
    Gate.init()

    val gDocs = docs map Runagate.factorie2Gate

    Gate.getCreoleRegister.registerDirectories(new File("/Users/johnsullivan/git/anzo/anzo/com.cambridgesemantics.anzo.unstructured/OSGI-INF/Gate/gate-6.1-build3913-ALL/plugins", "ANNIE").toURI.toURL)

    val controller = GateFactory.createResource("gate.creole.SerialAnalyserController", GateFactory.newFeatureMap(), GateFactory.newFeatureMap(), "ANNIE_" + Gate.genSym()).asInstanceOf[SerialAnalyserController]
    controller.add(GateFactory.createResource("gate.creole.annotdelete.AnnotationDeletePR").asInstanceOf[ProcessingResource])
    ANNIEConstants.PR_NAMES.foreach { preReq =>
      controller.add(GateFactory.createResource(preReq, GateFactory.newFeatureMap()).asInstanceOf[ProcessingResource])
    }


    println("Starting run of %d docs".format(docs.length))
    val times = new mutable.ArrayBuffer[Double]()
    var idx = 0
    (0 until 50) foreach {_ =>
      val corpus = GateFactory.newCorpus("gate.corpora.CorpusImpl")
      gDocs foreach corpus.asInstanceOf[java.util.List[gate.Document]].add
      controller setCorpus corpus
      val startTime = System.currentTimeMillis()
      controller.execute()
      val endTime = System.currentTimeMillis()
      times += (endTime - startTime)/1000.0
      idx += 1
      println("run %d took %.4f secs".format(idx, times.last))
    }
    println("Average run: %.4f secs".format(times.sum / times.length))


  }

}

object FactorieSpeedTest {

  def setUp(implicit mp:ModelProvider[ConllChainNer], lexicons:LexiconsProvider): DocumentAnnotator = {

    new DocumentAnnotator {
      def postAttrs = ???
      def prereqAttrs = ???
      println(ModelProvider.providerFor[ConllChainNer])
      private val ner = new WellFormedNer(new ConllChainNer()(mp, new StaticLexicons()(lexicons)))
      //ner.createChunks = false

      def process(document: FacDocument) =
        BilouConllNerChunkAnnotator.process(
          ner.process(
            DeterministicSentenceSegmenter.process(
              DeterministicTokenizer.process(document))))

      def tokenAnnotationString(token: Token) = null
    }
  }

  val opts = new SpeedTestCmdOpts {}


  def main(args:Array[String]): Unit = {
    opts parse args
    import opts._

    val anno = setUp(nerModel.value, lexiconFile.value)
    val preDocs = new LoadConll2003(true, true).fromFile(conllData.value)

    println("Starting run of %d docs".format(preDocs.length))
    val times = new mutable.ArrayBuffer[Double]()
    var idx = 0
    (0 until 1) foreach {_ =>
      val docIter = preDocs.map(d => new FacDocument(d.string)).iterator
      val startTime = System.currentTimeMillis()
      while(docIter.hasNext) {
        val doc = docIter.next()
        anno.process(doc)
        print(".")
      }
      val endTime = System.currentTimeMillis()
      times += (endTime - startTime)/1000.0
      idx += 1
      println("run %d took %.4f secs".format(idx, times.last))
    }
    println("Average run: %.4f secs".format(times.sum / times.length))
    println("Tail Average: %.4f secs".format(times.tail.sum / (times.length - 1)))

  }
}

object TestWellFormedNer {
  val opts = new SpeedTestCmdOpts {
    val outDir = new CmdOption[File]("output-dir", new File("conll-out"), "DIR", "")
  }

  def main(args:Array[String]): Unit = {
    opts parse args
    import opts._

    val docs = new LoadConll2003(true, false).fromFile(conllData.value)

    val tagger = new ConllChainNer()(nerModel.value, new StaticLexicons()(lexiconFile.value))
    val wfTagger = new WellFormedNer(tagger)

    docs.foreach(tagger.process)

    docs.foreach{ doc =>
      val tokLines = doc.tokens.toSeq.map { t =>
        t.string + "\t" + t.attr[BilouConllNerTag].categoryValue
      }
      wfTagger process doc
      BilouConllNerChunkAnnotator process doc
      val docString = tokLines.zip(doc.tokens.toSeq.map(_.attr[BilouConllNerTag].categoryValue)).map{case (x,y) => x + "\t" + y}.mkString("\n")
      val wrt = new BufferedWriter(new FileWriter(new File(outDir.value, doc.name)))
      wrt.write(docString)
      wrt.flush()
      wrt.close()
    }

  }
}