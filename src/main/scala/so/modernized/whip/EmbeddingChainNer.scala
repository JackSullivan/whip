package so.modernized.whip

import java.io.InputStream

import cc.factorie.app.chain.{LiteChainModelExample, SegmentEvaluation, ChainHelper}
import cc.factorie.app.nlp.{lexicon => lex, Document, Token}
import cc.factorie.app.nlp.lexicon.{LexiconsProvider, StaticLexicons}
import cc.factorie.app.nlp.ner.{SpanEncoding, NerTag, ChainNer}
import cc.factorie.la.{DenseTensor1, Tensor1}
import cc.factorie.optimize.{Trainer, ParameterAveraging, AdaGrad}
import cc.factorie.variable._
import cc.factorie.util._
import edu.umass.cs.iesl.entity_embeddings.data_structures._
import edu.umass.cs.iesl.entity_embeddings.data_structures.data_stores.EmbeddingCollection

import scala.reflect.ClassTag

/**
 * @author johnsullivan
 */
abstract class EmbeddingChainNer[L <: NerTag](ld:CategoricalDomain[String] with SpanEncoding,
                                              nl:(Token, String) => L,
                                              ltt:L => Token,
                                              modelIs:InputStream,
                                              l:LexiconsProvider,
                                              val embeddingColl:EmbeddingCollection)(implicit m:ClassTag[L])
  extends ChainNer[L](ld, nl, ltt, modelIs, new StaticLexicons()(l))(m){

  def lexiconMap(lp:LexiconsProvider) = {

    Iterator(
      lp.provide[lex.iesl.Month] -> "MONTH",
      lp.provide[lex.iesl.Day] -> "DAY",

      lp.provide[lex.iesl.PersonFirstHigh] -> "PERSON-FIRST-HIGH",
      lp.provide[lex.iesl.PersonFirstHighest] -> "PERSON-FIRST-HIGHEST",
      lp.provide[lex.iesl.PersonFirstMedium] -> "PERSON-FIRST-MEDIUM",

      lp.provide[lex.iesl.PersonLastHigh] -> "PERSON-LAST-HIGH",
      lp.provide[lex.iesl.PersonLastHighest] -> "PERSON-LAST-HIGHEST",
      lp.provide[lex.iesl.PersonLastMedium] -> "PERSON-LAST-MEDIUM",

      lp.provide[lex.iesl.PersonHonorific] -> "PERSON-HONORIFIC",

      lp.provide[lex.iesl.Company] -> "COMPANY",
      lp.provide[lex.iesl.JobTitle] -> "JOB-TITLE",
      lp.provide[lex.iesl.OrgSuffix] -> "ORG-SUFFIX",

      lp.provide[lex.iesl.Country] -> "COUNTRY",
      lp.provide[lex.iesl.City] -> "CITY",
      lp.provide[lex.iesl.PlaceSuffix] -> "PLACE-SUFFIX",
      lp.provide[lex.iesl.UsState] -> "USSTATE",
      lp.provide[lex.iesl.Continents] -> "CONTINENT",

      lp.provide[lex.wikipedia.Person] -> "WIKI-PERSON",
      lp.provide[lex.wikipedia.Event] -> "WIKI-EVENT",
      lp.provide[lex.wikipedia.Location] -> "WIKI-LOCATION",
      lp.provide[lex.wikipedia.Organization] -> "WIKI-ORG",
      lp.provide[lex.wikipedia.ManMadeThing] -> "MANMADE",
      lp.provide[lex.iesl.Demonym] -> "DEMONYM",

      lp.provide[lex.wikipedia.Book] -> "WIKI-BOOK",
      lp.provide[lex.wikipedia.Business] -> "WIKI-BUSINESS",
      lp.provide[lex.wikipedia.Film] -> "WIKI-FILM"
    ).map{case (mp, n) => n -> EntitySlug(EnrichRedirectsAndVocab.fixName(mp.coordinates), English).normalizedSlug }.toMap
  }

  class ChainNerWithEmbeddingFeatures(val token:Token, val feats:ChainNERFeatures) extends TensorVar {
    type Value = ImmutableConcatTensor1

    val lexMap = lexiconMap(l)

    lazy val embedding = {
      (feats.activeCategories.flatMap(lexMap.get).map(embeddingColl.getEmbedding(_, Output, CoOccurringEntity)) ++
        Seq(embeddingColl.getEmbedding(WordSlug(token.lemmaString, English).normalizedSlug, Output, CoOccurringEntity),
          embeddingColl.getEmbedding(token.lemmaString, Output, CoOccurringEntity),
          Some(new DenseTensor1(embeddingColl.outputDims))
        )).flatten.reduce(_ + _)
    }.asInstanceOf[Tensor1]

    def value: Value = ImmutableConcatTensor1(feats.value, embedding)
  }

  val liteModel = new HybridChainModel[L](labelDomain, ChainNERFeaturesDomain.dimensionDomain, embeddingColl.outputDims, {label:L => label.token.attr[ChainNerWithEmbeddingFeatures].value.tuple})

  override def addFeatures(document:Document, vf:Token => CategoricalVectorVar[String]): Unit = {
    super.addFeatures(document, vf)
    document.tokens foreach { t =>
      t.attr += new ChainNerWithEmbeddingFeatures(t, t.attr[ChainNERFeatures])
    }
  }

  override def process(document:Document) =
    if(document.tokenCount > 0) {
      if (!document.tokens.head.attr.contains(m.runtimeClass))
        document.tokens.map(token => token.attr += newLabel(token, "O"))
      if (!document.tokens.head.attr.contains(classOf[ChainNERFeatures])) {
        document.tokens.map(token => {token.attr += new ChainNERFeatures(token)})
        addFeatures(document, (t:Token)=>t.attr[ChainNERFeatures])
      }
      document.sentences.collect {
        case sentence if sentence.nonEmpty =>
          val vars = sentence.tokens.map(_.attr[L]).toSeq
          val result = ChainHelper.viterbiFast(liteModel.potentials(sentence.tokens.map(_.attr[L]).toSeq))
          vars.indices foreach (i => vars(i).set(result.mapValues(i))(null))
      }
      document
    } else {
      document
    }


  override def train(trainDocs: Seq[Document], testDocs: Seq[Document], rate: Double=0.18, delta: Double=0.066)(implicit random: scala.util.Random): Double = {

    def labels(docs: Iterable[Document]): Iterable[L with LabeledMutableDiscreteVar] = {
      docs.flatMap(doc => doc.tokens.map(_.attr[L with LabeledMutableDiscreteVar]))
    }

    println("initializing training features...")
    (trainDocs ++ testDocs).foreach(_.tokens.map(token => token.attr += new ChainNERFeatures(token)))
    trainDocs.foreach(addFeatures(_, (t:Token)=>t.attr[ChainNERFeatures]))
    ChainNERFeaturesDomain.freeze()
    println("initializing testing features...")
    testDocs.foreach(addFeatures(_, (t:Token)=>t.attr[ChainNERFeatures]))
    println(sampleOutputString(trainDocs.take(20).last.tokens.take(100)))

    val trainLabels = labels(trainDocs).toIndexedSeq
    val testLabels = labels(testDocs).toIndexedSeq
    val labelDomain: CategoricalDomain[String] = trainLabels.head.domain.asInstanceOf[CategoricalDomain[String]]
    (trainLabels ++ testLabels).foreach(_.setRandomly)

    val examples = trainDocs.flatMap(_.sentences.filter(_.length > 1).map(sentence => new LiteChainModelExample[L](liteModel, sentence.tokens.map(_.attr[L with LabeledDiscreteVar]), y => y.value)))
    val optimizer = new AdaGrad(rate=rate, delta=delta) with ParameterAveraging

    def evaluate(){
      val segmentEvaluation = new SegmentEvaluation[L with CategoricalLabeling[String]](
        labelDomain.categories.filter(_.length > 2).map(_.substring(2)),
        "(B|U)-", "(I|L)-"
      )
      trainDocs.foreach(doc => {
        process(doc)
        for (sentence <- doc.sentences) segmentEvaluation += sentence.tokens.map(_.attr[L with CategoricalLabeling[String]])
      })
      println(s"Train accuracy ${objective.accuracy(trainLabels)}")
      println(segmentEvaluation)
      if (testDocs.nonEmpty) {
        val testSegmentEvaluation = new SegmentEvaluation[L with LabeledMutableCategoricalVar[String]](
          labelDomain.categories.filter(_.length > 2).map(_.substring(2)),
          "(B|U)-", "(I|L)-"
        )
        testDocs.foreach(doc => {
          process(doc)
          for (sentence <- doc.sentences) testSegmentEvaluation += sentence.tokens.map(_.attr[L with CategoricalLabeling[String]])
        })
        println(s"Test accuracy ${objective.accuracy(testLabels)}")
        println(testSegmentEvaluation)
      }
      println(liteModel.parameters.tensors.map(t => t.toSeq.count(x => x == 0)).sum.toFloat/liteModel.parameters.tensors.map(_.length).sum +" sparsity")
    }

    println(s"training with ${examples.length} examples")
    Trainer.onlineTrain(liteModel.parameters, examples, optimizer=optimizer, evaluate=evaluate)

    val finalEval = new SegmentEvaluation[L with LabeledMutableCategoricalVar[String]](labelDomain.categories.filter(_.length > 2).map(_.substring(2)), "(B|U)-", "(I|L)-")
    val buf = new StringBuffer
    buf.append(new LabeledDiscreteEvaluation(testDocs.flatMap(_.tokens.map(_.attr[L with LabeledMutableDiscreteVar]))))
    for (doc <- testDocs; sentence <- doc.sentences) finalEval += sentence.tokens.map(_.attr[L with LabeledMutableCategoricalVar[String]])
    println("final results:")
    println(finalEval)
    finalEval.f1
  }
}
