package so.modernized.whip

import java.io.File
import java.net.URL
import java.util.concurrent.atomic.AtomicInteger

import cc.factorie.app.nlp.lexicon.{StaticLexicons, Lexicon}
import cc.factorie.app.nlp.{DocumentAnnotator, Document}
import cc.factorie.app.nlp.ner.{BilouConllNerDomain, BilouConllNerTag, ConllChainNer}
import cc.factorie.app.nlp.phrase.DatePhraseFinder
import cc.factorie.app.nlp.phrase.DatePhraseFinder.{DatePhrase, DatePhraseList}
import cc.factorie.app.nlp.segment.{DeterministicTokenizer, DeterministicSentenceSegmenter}
import cc.factorie.util.ModelProvider._
import cc.factorie.util.ModelProvider
import org.apache.commons.io.FileUtils
import scala.StringBuilder
import scala.collection.mutable

/**
 * @author johnsullivan
 */
case class LightAnnotation(documentId:String, startIdx:Int, endIdx:Int, label:String) extends Ordered[LightAnnotation] {
  override def compare(that: LightAnnotation): Int = if(this.documentId == that.documentId) {
    if(this.endIdx < that.startIdx) {
      -1
    } else if (this.startIdx > that.endIdx) {
      1
    } else if (this.startIdx < that.startIdx) {
      -1
    } else if (this.startIdx > that.startIdx) {
      1
    } else {
      0
    }
  } else {
    throw new IllegalArgumentException("both annotations must come from the same document")
  }
}

object NERTest {

  def process(da:DocumentAnnotator, doc:Document) = {
    DeterministicTokenizer.process(doc)
    DeterministicSentenceSegmenter.process(doc)
    da.process(doc)
    DatePhraseFinder.process(doc)
  }

  def getFilesWhere(dir:File, pred:File => Boolean={_ => true}):Seq[File] = if(dir.isDirectory) {
    dir.listFiles().flatMap(getFilesWhere(_, pred))
  } else {
    if (pred(dir)) Seq(dir) else Seq.empty[File]
  }

  def extractAnnotations(doc:Document):Seq[LightAnnotation] = {

    val annos = new mutable.ArrayBuffer[LightAnnotation]
    val tokIter = doc.tokens.iterator

    var currentStart = -1
    while(tokIter.hasNext) {
      val tok = tokIter.next()
      val tokLabel = tok.attr[BilouConllNerTag]
      val catVal = tokLabel.categoryValue
      if(catVal.startsWith("B-")) {
        currentStart = tok.stringStart
      }
      if(catVal.startsWith("L-")) {
        if(currentStart == -1) {
          throw new IllegalStateException("We didn't expect to be here: " + (tok.prevWindow(3) ++ Seq(tok) ++ tok.nextWindow(3)).map(t => t.string + ":" + t.attr[BilouConllNerTag].categoryValue).mkString(" "))
        }
        annos += LightAnnotation(doc.name, currentStart, tok.stringEnd, tokLabel.baseCategoryValue)
        currentStart = -1
      }
      if(catVal.startsWith("U")) {
        annos += LightAnnotation(doc.name, tok.stringStart, tok.stringEnd, tokLabel.baseCategoryValue)
        currentStart = -1
      }
      if(catVal.startsWith("O")) {
        currentStart = -1
      }
    }
    val dpIter = doc.attr[DatePhraseList].iterator
    while(dpIter.hasNext) {
      val datePhrase = dpIter.next().asInstanceOf[DatePhrase]
      datePhrase.head.stringStart
      annos += LightAnnotation(doc.name, datePhrase.head.stringStart, datePhrase.last.stringEnd, "%d-%d-%d".format(datePhrase.year, datePhrase.month, datePhrase.day))
    }

    annos
  }

  def renderDocument(doc:Document, annos:Seq[LightAnnotation]):String = {
    val annoIter = annos.sorted.iterator
    val render = new StringBuilder(s"<h1>${doc.name}</h1>\n")
    var currentIdx = 0
    while(annoIter.hasNext) {
      val anno = annoIter.next()
      render.append(doc.string.substring(currentIdx, anno.startIdx))
      render.append("<a href=\"%s\" >".format(anno.label))
      render.append(doc.string.substring(anno.startIdx, anno.endIdx))
      render.append("</a>")
      currentIdx = anno.endIdx
    }
    render.toString()
  }

  def main(args:Array[String]): Unit = {
    println("In Main")

    val rootDir = args(0)
    implicit val mp:ModelProvider[ConllChainNer] = new File(args(1))
    implicit val lexicons:StaticLexicons = new StaticLexicons()(new File(args(2)))

    println(providerFor[ConllChainNer])
    val ner = new ConllChainNer

    val files = getFilesWhere(new File(rootDir))
    println("Found %d files to process".format(files.size))

    //val iter = files.iterator

    val parFile = files.par
    val idx = new AtomicInteger(0)
    var time = System.currentTimeMillis()

    parFile.foreach { f =>
      val doc = process(ner, new Document(FileUtils.readFileToString(f)).setName(f.getName))
      val annos = extractAnnotations(doc)
      FileUtils.write(new File(f.getName + ".html"), renderDocument(doc, annos))
      print(".")
      val curIdx = idx.incrementAndGet()
      if(curIdx % 100 == 0) {
        val curTime = System.currentTimeMillis()
        println("Processed %d docs in %.3f secs".format(curIdx, (curTime - time)/1000.0))
        time = System.currentTimeMillis()
      }
    }
    /*
    while(iter.hasNext) {
      val f = iter.next()
      val doc = process(new Document(FileUtils.readFileToString(f)).setName(f.getName))
      print(".")
      idx += 1
      if(idx % 100 == 0) {
        val curTime = System.currentTimeMillis()
        (0 until 100) foreach (_ => print("\b"))
        println("Processed %d docs in %.3f secs".format(idx, (curTime - time)/1000.0))
        time = System.currentTimeMillis()
      }
    }
    */
    val curTime = System.currentTimeMillis()
    println()
    println("Processed %d docs in %.3f secs".format(idx.get(), (curTime - time)/1000.0))
  }
}
