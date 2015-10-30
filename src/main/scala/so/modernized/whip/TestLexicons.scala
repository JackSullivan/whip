package so.modernized.whip

import java.io.File

import cc.factorie.app.nlp.lexicon.{LexiconsProvider, StaticLexicons}
import cc.factorie.util.{ModelProviderCmdOptions, DefaultCmdOptions, CmdOptions}

/**
 * @author johnsullivan
 */
object TestLexicons {
  val opts = new CmdOptions with DefaultCmdOptions with ModelProviderCmdOptions {
    val lexicons = new CmdOption[File]("lexicons", new File(""), "DIR", "")
  }

  def main(args:Array[String]): Unit = {
    opts parse args
    import opts._

    val lex = new StaticLexicons()(LexiconsProvider.fromFile(lexicons.value, true))

    lex.iesl.Continents.toString()
    lex.iesl.Country.toString()
    lex.iesl.City.toString()
    lex.iesl.UsState.toString()
    lex.iesl.PlaceSuffix.toString()
    lex.iesl.JobTitle.toString()
    lex.iesl.Money.toString()
    lex.iesl.Company.toString()
    lex.iesl.OrgSuffix.toString()
    lex.iesl.Month.toString()
    lex.iesl.Day.toString()
    lex.iesl.PersonHonorific.toString()
    lex.iesl.PersonFirstHighest.toString()
    lex.iesl.PersonFirstHigh.toString()
    lex.iesl.PersonFirstMedium.toString()
    lex.iesl.PersonLastHighest.toString()
    lex.iesl.PersonLastHigh.toString()
    lex.iesl.PersonLastMedium.toString()
    lex.iesl.Say.toString()
    lex.iesl.Demonym.toString()
    lex.iesl.DemonymMap.toString()
    lex.iesl.AllPlaces.toString()
    lex.iesl.PersonFirst.toString()
    lex.iesl.PersonLast.toString()
    lex.iesl.PersonFirstHighest.toString()
    lex.iesl.PersonFirstHigh.toString()
    lex.iesl.PersonFirstMedium.toString()
    lex.iesl.PersonLastHighest.toString()
    lex.iesl.PersonLastHigh.toString()
    lex.iesl.PersonLastMedium.toString()
    lex.iesl.PersonFirst.toString()
    lex.iesl.PersonLast.toString()
    lex.iesl.PersonLast.toString()

    /*
    lex.ssdi.PersonFirstHighest.toString()
    lex.ssdi.PersonFirstHigh.toString()
    lex.ssdi.PersonFirstMedium.toString()
    lex.ssdi.PersonLastHighest.toString()
    lex.ssdi.PersonLastHigh.toString()
    lex.ssdi.PersonLastMedium.toString()
    lex.ssdi.PersonFirst.toString()
    lex.ssdi.PersonLast.toString()
    lex.ssdi.PersonFirstHighest.toString()
    lex.ssdi.PersonFirstHigh.toString()
    lex.ssdi.PersonFirstMedium.toString()
    lex.ssdi.PersonLastHighest.toString()
    lex.ssdi.PersonLastHigh.toString()
    lex.ssdi.PersonLastMedium.toString()
    lex.ssdi.PersonFirst.toString()
    lex.ssdi.PersonLast.toString()
    lex.ssdi.PersonLast.toString()
    */

    lex.uscensus.PersonLast.toString()
    lex.uscensus.PersonFirstFemale.toString()
    lex.uscensus.PersonFirstMale.toString()

    lex.wikipedia.Battle.toString()
    lex.wikipedia.BattleRedirect.toString()
    lex.wikipedia.BattleAndRedirect.toString()
    lex.wikipedia.BattleDisambiguation.toString()
    lex.wikipedia.Book.toString()
    lex.wikipedia.BookRedirect.toString()
    lex.wikipedia.BookAndRedirect.toString()
    lex.wikipedia.BookDisambiguation.toString()
    lex.wikipedia.Business.toString()
    lex.wikipedia.BusinessRedirect.toString()
    lex.wikipedia.BusinessAndRedirect.toString()
    lex.wikipedia.BusinessDisambiguation.toString()
    lex.wikipedia.Competition.toString()
    lex.wikipedia.CompetitionRedirect.toString()
    lex.wikipedia.CompetitionAndRedirect.toString()
    lex.wikipedia.CompetitionDisambiguation.toString()
    lex.wikipedia.Event.toString()
    lex.wikipedia.EventRedirect.toString()
    lex.wikipedia.EventAndRedirect.toString()
    lex.wikipedia.EventDisambiguation.toString()
    lex.wikipedia.Film.toString()
    lex.wikipedia.FilmRedirect.toString()
    lex.wikipedia.FilmAndRedirect.toString()
    lex.wikipedia.FilmDisambiguation.toString()
    lex.wikipedia.Location.toString()
    lex.wikipedia.LocationRedirect.toString()
    lex.wikipedia.LocationAndRedirect.toString()
    lex.wikipedia.LocationDisambiguation.toString()
    lex.wikipedia.ManMadeThing.toString()
    lex.wikipedia.ManMadeThingRedirect.toString()
    lex.wikipedia.ManMadeThingAndRedirect.toString()
    lex.wikipedia.ManMadeThingDisambiguation.toString()
    lex.wikipedia.Organization.toString()
    lex.wikipedia.OrganizationRedirect.toString()
    lex.wikipedia.OrganizationAndRedirect.toString()
    lex.wikipedia.OrganizationDisambiguation.toString()
    lex.wikipedia.Person.toString()
    lex.wikipedia.PersonRedirect.toString()
    lex.wikipedia.PersonAndRedirect.toString()
    lex.wikipedia.PersonDisambiguation.toString()
    lex.wikipedia.Song.toString()
    lex.wikipedia.SongRedirect.toString()
    lex.wikipedia.SongAndRedirect.toString()
    lex.wikipedia.SongDisambiguation.toString()


    lex.mandarin.SurnamePinyin.toString()
    lex.mandarin.GivenNamePinyin.toString()


  }

}
