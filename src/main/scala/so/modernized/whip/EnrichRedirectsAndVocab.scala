package so.modernized.whip

import java.io.{BufferedWriter, FileWriter, File}

import cc.factorie.app.nlp.lexicon
import cc.factorie.app.nlp.lexicon.{TriePhraseLexicon, Lexicon, LexiconsProvider, StaticLexicons}
import cc.factorie.util.{DefaultCmdOptions, ModelProviderCmdOptions}
import cc.factorie.variable.CategoricalDomain
import edu.umass.cs.iesl.entity_embeddings.data_structures._
import edu.umass.cs.iesl.entity_embeddings.data_structures.data_stores.{SurfaceFormDB, TypeDB}
import edu.umass.cs.iesl.entity_embeddings.embedding.contexts.CoOccurringEntityContext

import edu.umass.cs.iesl.entity_embeddings.embedding.{EntityCBOWEmbedder, Vocabulary, EntityResolver, EmbedderOpts}
import edu.umass.cs.iesl.entity_embeddings.load.LoadEmbeddingExamplesWikipedia

import cc.factorie.util.ISAble._

import scala.collection
import scala.util.Random

/*
class LexiconEntityResolver(l:Lexicon) extends EntityResolver {
  override def resolve(entString:String):Option[String] = if(l contains entString) {
    Some(l.getClass.getName)
  } else None

  def <>(that:EntityResolver):EntityResolver = new EntityResolver {
    override def resolve(entString:String):Option[String] =
      this.resolve(entString) orElse that.resolve(entString)
  }
}
*/


/**
 * @author johnsullivan
 */
object EnrichRedirectsOpts extends DefaultCmdOptions with ModelProviderCmdOptions {
  val inRedirects = new CmdOption("in-redirects", new File(""), "FILE", "")
  val inVocabulary = new CmdOption("in-vocabulary", new File(""), "FILE", "")

  val outRedirects = new CmdOption("out-redirects", new File(""), "FILE", "")
  val outVocabulary = new CmdOption("out-vocabulary", new File(""), "FILE", "")

  val outSurfaceForm = new CmdOption("out-surface-form", new File(""), "FILE", "")

  val lp = new LexiconsProviderCmdOption("lexicon-path")

  val lang = new CmdOption("iso-lang", "en", "ISO-Language", "")
}

class Counter(private var nextIdx:Int=0) extends (() => Int) {
  def apply():Int = {
    val res = nextIdx
    nextIdx += 1
    res
  }
}

// reads the redirects file and lexicon provider, writes the result
object EnrichRedirectsAndVocab {
  val opts = EnrichRedirectsOpts

  def lexicons(lp:LexiconsProvider) = {

    Iterator(
      lp.provide[lexicon.iesl.Month],
      lp.provide[lexicon.iesl.Day],

      lp.provide[lexicon.iesl.PersonFirstHigh],
      lp.provide[lexicon.iesl.PersonFirstHighest],
      lp.provide[lexicon.iesl.PersonFirstMedium],

      lp.provide[lexicon.iesl.PersonLastHigh],
      lp.provide[lexicon.iesl.PersonLastHighest],
      lp.provide[lexicon.iesl.PersonLastMedium],

      lp.provide[lexicon.iesl.PersonHonorific],

      lp.provide[lexicon.iesl.Company],
      lp.provide[lexicon.iesl.JobTitle],
      lp.provide[lexicon.iesl.OrgSuffix],

      lp.provide[lexicon.iesl.Country],
      lp.provide[lexicon.iesl.City],
      lp.provide[lexicon.iesl.PlaceSuffix],
      lp.provide[lexicon.iesl.UsState],
      lp.provide[lexicon.iesl.Continents],

      lp.provide[lexicon.wikipedia.Person],
      lp.provide[lexicon.wikipedia.Event],
      lp.provide[lexicon.wikipedia.Location],
      lp.provide[lexicon.wikipedia.Organization],
      lp.provide[lexicon.wikipedia.ManMadeThing],
      lp.provide[lexicon.iesl.Demonym],

      lp.provide[lexicon.wikipedia.Book],
      lp.provide[lexicon.wikipedia.Business],
      lp.provide[lexicon.wikipedia.Film]
    )
  }

  def fixName(coords:String):String = coords.split("/").last.split("""\.""").head

  def main(args:Array[String]): Unit = {
    opts parse args

    val lang = DocLanguage.fromIsoString(opts.lang.value)

    val newRedirects = lexicons(opts.lp.value).flatMap { mp =>
      lines(mp.provide).map(w => WordSlug(w, lang) -> EntitySlug(mp.coordinates, lang))
    }.map{case (x,y) => x.normalizedSlug + "\t" + y.normalizedSlug}

    println("loaded lexicons, created new redirects")

    var wrt = new BufferedWriter(new FileWriter(opts.outRedirects.value))

    var i = 0
    lines(opts.inRedirects.value).foreach { l =>
      wrt.write(l)
      wrt.newLine()

      if(i % 1000 == 0) {
        print(".")
        wrt.flush()
      }
      if(i % 100000 == 0) {
        println()
      }
      i += 1
    }
    wrt.flush()

    println("\nrewrote original redirects, writing new ones")
    i = 0
    newRedirects.foreach { l =>
      wrt.write(l)
      wrt.newLine()
      if(i % 1000 == 0) {
        print(".")
        wrt.flush()
      }
      if(i % 100000 == 0) {
        println()
      }
    }
    wrt.flush()
    wrt.close()

    println("wrote new redirects")

    val maxVocab = lines(opts.inVocabulary.value).foldLeft(0){ case (acc, line) =>
      math.max(acc, line.split("""\s+""")(0).toInt)
    }
    println("found %s as max vocab".format(maxVocab))

    wrt = new BufferedWriter(new FileWriter(opts.outVocabulary.value))

    i = 0
    lines(opts.inVocabulary.value).foreach { l =>
      wrt.write(l)
      wrt.newLine()
      if(i % 1000 == 0) {
        print(".")
        wrt.flush()
      }
      if(i % 100000 == 0) {
        println()
      }
      i += 1
    }
    wrt.flush()

    println("\nrewrote original vocab, writing new")

    i = 0
    val nextId = new Counter(maxVocab)
    lexicons(opts.lp.value).flatMap { mp =>
      Seq(nextId() -> EntitySlug(fixName(mp.coordinates), lang).normalizedSlug).toIterator ++
        lines(mp.provide).map(nextId() -> WordSlug.apply(_, lang).normalizedSlug)
    }.foreach { case (id, slug) =>
      wrt.write("%s\t%s".format(id, slug))
      wrt.newLine()

      if(i % 1000 == 0) {
        print(".")
        wrt.flush()
      }
      if(i % 100000 == 0) {
        println()
      }
      i += 1
    }
    wrt.flush()
    wrt.close()

    println("wrote new vocab lines")

    wrt = new BufferedWriter(new FileWriter(opts.outSurfaceForm.value))
    i = 0

    lexicons(opts.lp.value).flatMap { mp =>
      lines(mp.provide).map(_ -> EntitySlug(fixName(mp.coordinates), lang).normalizedSlug)
    }.foreach { case (sf, slug) =>
      wrt.write("%s\t%s".format(sf, slug))
      wrt.newLine()
      if(i % 1000 == 0) {
        print(".")
        wrt.flush()
      }
      if(i % 100000 == 0) {
        println()
      }
      i += 1
    }
    wrt.flush()
    wrt.close()

    println("wrote surface forms")


  }

}

/*
trait EmbeddingLexiconOpts extends EmbedderOpts with ModelProviderCmdOptions {
  val lexiconProvider = new LexiconsProviderCmdOption("lexicon-path")
}

object NERTrain {
  def lexicons(lp:LexiconsProvider) = {
    val lexicon = new StaticLexicons()(lp)

    Seq(
      lexicon.iesl.Month,
      lexicon.iesl.Day,

      lexicon.iesl.PersonFirst,
      lexicon.iesl.PersonFirstHigh,
      lexicon.iesl.PersonFirstHighest,
      lexicon.iesl.PersonFirstMedium,

      lexicon.iesl.PersonLast,
      lexicon.iesl.PersonLastHigh,
      lexicon.iesl.PersonLastHighest,
      lexicon.iesl.PersonLastMedium,

      lexicon.iesl.PersonHonorific,

      lexicon.iesl.Company,
      lexicon.iesl.JobTitle,
      lexicon.iesl.OrgSuffix,

      lexicon.iesl.Country,
      lexicon.iesl.City,
      lexicon.iesl.PlaceSuffix,
      lexicon.iesl.UsState,
      lexicon.iesl.Continents,

      lexicon.wikipedia.Person,
      lexicon.wikipedia.Event,
      lexicon.wikipedia.Location,
      lexicon.wikipedia.Organization,
      lexicon.wikipedia.ManMadeThing,
      lexicon.iesl.Demonym,

      lexicon.wikipedia.Book,
      lexicon.wikipedia.Business,
      lexicon.wikipedia.Film,

      lexicon.wikipedia.LocationAndRedirect,
      lexicon.wikipedia.PersonAndRedirect,
      lexicon.wikipedia.OrganizationAndRedirect
    )
  }

  def main(args:Array[String]): Unit = {
    val opts = new EmbeddingLexiconOpts {}
    opts parse args

    val random = new Random(opts.seed.value)

    val resolver =
      lexicons(opts.lexiconProvider.value).map(l => new LexiconEntityResolver(l)).reduce(_ <> _)

    val typeDB:TypeDB = null
    val surfaceFormDB:SurfaceFormDB = null

    val cGen =
      new CoOccurringEntityContext(opts.cooccuringEntityWindowSize.value, opts.caseSensitiveMentions.value, typeDB, resolver, opts.alwaysAddTitle.value)(random)

    val contextDomain:CategoricalDomain[String] = null
    val targetDomain:CategoricalDomain[String] = null

    val embedder = new EntityCBOWEmbedder(
      opts.loss.value,
      contextDomain,
      targetDomain,
      opts.separateIO.value,
      opts.dims.value,
      opts.minContext.value,
      opts.normalizeX.value,
      opts.margin.value,
      opts.negativeNonEntity.value,
      opts.negativeSameSurfaceForm.value,
      opts.negativeRandom.value,
      surfaceFormDB,
      typeDB,
      opts.neverDiscardEntity.value,
      opts.useRegularization.value,
      opts.useHogWild.value,
      opts.numThreads.value)(random)

    val loader = new LoadEmbeddingExamplesWikipedia(
      opts.input.value.head,
      opts.maxWikiPages.value,
      Array(cGen),
      DocLanguage.fromIsoString(opts.inputFileLanguages.value.head),
      resolver,
      contextDomain,
      targetDomain,
      typeDB)

  }
}
*/
