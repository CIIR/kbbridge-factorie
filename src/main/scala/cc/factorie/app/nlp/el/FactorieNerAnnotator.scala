package cc.factorie.app.nlp.el

import cc.factorie.app.nlp._
import cc.factorie.app.chain.Lexicons
import cc.factorie.app.nlp.ner.{BilouConllNerLabel, ChainNerLabel}

/**
 * Created with IntelliJ IDEA.
 * User: jdalton
 * Date: 7/8/13
 * Time: 10:17 AM
 * To change this template use File | Settings | File Templates.
 */
object FactorieNERComponent extends DocumentAnnotator {

  def prereqAttrs: Iterable[Class[_]] = List(classOf[Sentence])
  def postAttrs: Iterable[Class[_]] = List(classOf[ChainNerLabel])
  override def tokenAnnotationString(token:Token): String = token.attr[ChainNerLabel].categoryValue

  import cc.factorie.app.nlp.ner._
  import cc.factorie.app.nlp.Token

  val baseModelDir = "models"
  val nerModelDir: String = baseModelDir + "/cc/factorie/app/ner/ChainNer2"

  ChainNer2.aggregate = true
  ChainNer2.twoStage = true
  ChainNer2.bP = true

  val brownClus = "models/cc/factorie/app/nlp/ner/brownBllipClusters"
  val lexiconDir = "models/cc/factorie/app/nlp/ner/KnownLists"

  val lexes = List("WikiArtWork.lst", "WikiArtWorkRedirects.lst", "WikiCompetitionsBattlesEvents.lst", "WikiCompetitionsBattlesEventsRedirects.lst", "WikiFilms.lst", "WikiFilmsRedirects.lst", "WikiLocations.lst", "WikiLocationsRedirects.lst", "WikiManMadeObjectNames.lst", "WikiManMadeObjectNamesRedirects.lst", "WikiOrganizations.lst", "WikiOrganizationsRedirects.lst", "WikiPeople.lst", "WikiPeopleRedirects.lst", "WikiSongs.lst", "WikiSongsRedirects.lst", "cardinalNumber.txt", "currencyFinal.txt", "known_corporations.lst", "known_country.lst", "known_jobs.lst", "known_name.lst", "known_names.big.lst", "known_nationalities.lst",  "known_state.lst", "known_title.lst", "measurments.txt", "ordinalNumber.txt", "temporal_words.txt")

  if(ChainNer2.clusters.size == 0) {
    for(line <- bufferedSourceFromFile(brownClus).getLines()){
      val splitLine = line.split("\t")
      ChainNer2.clusters(splitLine(1)) = splitLine(0)
    }
  }

  if(ChainNer2.lexicons == null) ChainNer2.lexicons = new Lexicons(lexes.map(x => (x,bufferedSourceFromFile(lexiconDir + "/" + x))))

  def bufferedSourceFromFile(f: String): scala.io.BufferedSource = {
    val diskFile = new java.io.File(f)
    if (diskFile.exists)
      scala.io.Source.fromFile(diskFile)
    else
      scala.io.Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream(f))
  }

  def process1(doc: Document): Document = {

    ChainNerFeaturesDomain.dimensionDomain.freeze()
    ChainNer2FeaturesDomain.dimensionDomain.freeze()
    println("is frozen: " + ChainNerFeaturesDomain.dimensionDomain.frozen)

    for(token <- doc.tokens) token.attr += new Conll2003ChainNerLabel(token, "O")

    doc.tokens.map(token => token.attr += new ChainNerFeatures(token))
    ChainNer2.initFeatures(doc,(t:Token)=>t.attr[ChainNerFeatures])

    println("is frozen: " + ChainNerFeaturesDomain.dimensionDomain.frozen)
    ChainNer2.process(doc)

    doc.tokens.map(token => token.attr += new ChainNer2Features(token))
    ChainNer2.initFeatures(doc,(t:Token)=>t.attr[ChainNer2Features])

    ChainNer2.initSecondaryFeatures(doc)

    ChainNer2.process(doc,true)

    for (s <- doc.sections) {
      for (t <- s.tokens) {
        println(t.toString())
        if (t.attr[ChainNerLabel].categoryValue != "O") {
          val attr = t.attr[ChainNerLabel].categoryValue.split("-")
          if (attr(0) == "U") {
            new NerSpan(s, attr(1), t.positionInSection, 1)(null)
          } else if (attr(0) == "B") {
            var lookFor = t
            while (lookFor.hasNext && lookFor.attr[ChainNerLabel].categoryValue.matches("(I|B)-" + attr(1))) lookFor = lookFor.next
            new NerSpan(s, attr(1), t.positionInSection, lookFor.positionInSection - t.positionInSection + 1)(null)
          }
        }
      }
    }
    doc
  }

}
