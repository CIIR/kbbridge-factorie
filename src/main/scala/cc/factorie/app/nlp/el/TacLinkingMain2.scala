package cc.factorie.app.nlp.el

import java.io.{StringReader, File}
import org.lemurproject.galago.tupleflow.Parameters
import org.lemurproject.galago.core.retrieval.{ScoredDocument, Retrieval, RetrievalFactory}
import edu.umass.ciir.kbbridge.tac.TacQueryUtil
import cc.factorie.app.nlp.pos._
import edu.umass.ciir.kbbridge.data.{IdMap, SimpleEntityMention, ScoredWikipediaEntity, TacEntityMention}
import cc.factorie.app.nlp.ner.NER3NoEmbeddings
import cc.factorie.app.nlp.mention.{MentionEntityType, MentionType, NerAndPronounMentionFinder}
import cc.factorie.app.nlp.{Document, DocumentAnnotatorPipeline, MutableDocumentAnnotatorMap}
import org.xml.sax.InputSource
import scala.xml.XML
import edu.umass.ciir.kbbridge.util.{NameDictionary, KbBridgeProperties}
import edu.umass.ciir.kbbridge.text2kb.{GalagoDoc2WikipediaEntity, QVMLocalTextEntityRepr}
import edu.umass.ciir.kbbridge.RankLibReranker
import edu.umass.ciir.kbbridge.nlp.NlpData.NlpXmlNerMention
import edu.umass.ciir.kbbridge.search.{DocumentBridgeMap, EntityRetrievalWeighting, EntityReprRetrieval}
import edu.umass.ciir.kbbridge.tac.TacFullyLinkedEval.LinkedMention
import edu.umass.ciir.galago.{GalagoSearcher, GalagoQueryLib}
import edu.umass.ciir.kbbridge.data.repr.EntityRepr

/**
 * Created with IntelliJ IDEA.
 * User: jdalton
 * Date: 9/11/13
 * Time: 3:33 PM
 * To change this template use File | Settings | File Templates.
 */
object TacLinkingMain2 extends App {

  val testMode = true
  var tacQueryIds: Set[String] = args(0).split(",").toSet
  val outputDir: File = new File(args(2))
  val inputNlpDir = args(3)
  val useUBExpasion = args(4).toBoolean
  val urbanDictionaryIdxPath = args(5)


  val tacQueries = TacQueryUtil.allQueries()

  println("starting to link queries: " + tacQueryIds)
  val testQueries = if (tacQueryIds.size > 0) {
    tacQueries.filter(q => tacQueryIds contains q.mentionId)
  } else {
    tacQueries
  }
  println("queries: " + testQueries.size)

  //outputDir = new File(args(2))
  if (!outputDir.exists()) {
    outputDir.mkdirs()
  }

  val p = new Parameters()
  p.set("index", args(1))
  if (testMode || workTodo(testQueries)) {
    p.set("terms", true)
    p.set("tags", true)
    val retrieval = RetrievalFactory.instance(p)
    println("Starting annotation:")
    annotateMentions(testQueries, retrieval)
  }

  def workTodo(mentions: Seq[TacEntityMention]): Boolean = {
    var workTodo = false
    for (mention <- mentions) {
      val outputFile = new File(outputDir.getAbsolutePath + File.separator + mention.mentionId + ".xml")
      if (!outputFile.exists()) {
        workTodo = true
      }
    }
    workTodo
  }

  def annotateMentions(mentions: Seq[TacEntityMention], retrieval: Retrieval) = {

    val candidateGenerator = DocumentBridgeMap.getKbRetrieval
    val reranker = new RankLibReranker(KbBridgeProperties.rankerModelFile)

    for (m <- mentions) {
      val outputFile = new File(outputDir.getAbsolutePath + File.separator + m.mentionId + ".xml")

      if (testMode || !outputFile.exists()) {

        val file = new File(inputNlpDir + File.separatorChar + m.docId + ".xml")
        val (query, annotatedMention) = if (file.exists()) {
          val annotatedMention = annotationsFromFile(m, file)
          val repr = QVMLocalTextEntityRepr.createEntityRepr(annotatedMention)
          val expandedRepr = if (useUBExpasion) {
            urbanDictionaryNames(m, repr)
          } else {
            repr
          }
          val query = if (expandedRepr.nameVariants.size > 0) {
            val queryString = EntityReprRetrieval.buildRawQuery(expandedRepr,EntityRetrievalWeighting(0.31, 0.38, 0.0, 0.31), false, 0)
            queryString
          } else {
            val queryString = EntityReprRetrieval.buildRawQuery(expandedRepr,EntityRetrievalWeighting(0.7, 0.0, 0.0, 0.3), false, 0)
            queryString
          }
          (query, annotatedMention)
        } else {
          println("annotation file does not exist, using name query")
          val galagoQuery = GalagoQueryLib.buildSeqDepForString(m.entityName, Seq(), false, 0)
          (galagoQuery, m)
        }

        println("Fetching candidates for mention: " + m.mentionId + " d:" + m.docId + " name:" + m.entityName)
        val numCands =  250
        //println("running query: " + query + " num cands: " + numCands)

        val t0 = System.currentTimeMillis
        val retrievedCands = candidateGenerator.retrieveScoredDocuments(query, numCands)
        val t1 = System.currentTimeMillis()
        val diff = t1-t0
        println(s"Query: $query time: $diff cands: $numCands")


        // ugly hack alert.  retrieval fails on state abbreviations quite often.
        val normalizedName = m.entityName.toLowerCase().replaceAll("[^a-z01-9 ]", "")
        val commonName = NameDictionary.COMMON_PLACE_ABBR_MAP.get(normalizedName)
        val finalCands = commonName match {
          case Some(nameMatch) => {
               val docs = retrievedCands.map(_.documentName).toSet
               if (!docs.contains(nameMatch)) {
                  println("adding name abbreviation match.")
                  val newCands = retrievedCands ++ Seq(new ScoredDocument(nameMatch, 5, -1))
                GalagoDoc2WikipediaEntity.galagoResultToWikipediaEntities(newCands)
               } else {
                GalagoDoc2WikipediaEntity.galagoResultToWikipediaEntities(retrievedCands)
               }
          }
          case None => GalagoDoc2WikipediaEntity.galagoResultToWikipediaEntities(retrievedCands)
        }

        val t2 = System.currentTimeMillis
        val rerankedResults = reranker.rerankCandidatesGenerateFeatures(annotatedMention, finalCands).toSeq
        val t3 = System.currentTimeMillis()
        val diff2 = t3 - t2
        println(s"Reranking time: $diff2")

        val xml = LinkedMention2XmlRenderer.xml(annotatedMention, rerankedResults, finalCands)
        //println(xml.toString)

        println("writing results: " + outputFile.getAbsolutePath)
        XML.save(outputFile.getAbsolutePath, xml, "UTF-8")

      }


      //        val xml = new toXml(doc, links)
      //        println(xml.makeDocument.toString)
      //        xml.save(outputFile)
    }

  }


  def urbanDictionaryNames(m: TacEntityMention, repr: EntityRepr) = {
    val retrieval : GalagoSearcher = GalagoSearcher(urbanDictionaryIdxPath)
    val urbanDictEntries = retrieval.getDocument(m.mentionId, p)

    if (urbanDictEntries != null) {
      println("Using urban dictionary entry.")
      val parser = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl().newSAXParser()
      val adapter = new scala.xml.parsing.NoBindingFactoryAdapter
      val xmlDoc = adapter.loadXML(new InputSource(new StringReader(urbanDictEntries.text)), parser)

      val tags = xmlDoc \\ "tags"
      val tagMap = tags.map(t => EntityRepr(entityName = t.text.trim(), queryId = None) -> (1.0 / (tags.size + repr.neighbors.size)))
      val newNeighbors = repr.neighbors ++ tagMap
      println("Original repr:" + repr)
      val newRepr = repr.copy(neighbors = newNeighbors)
      println("New repr:" + newRepr)
      newRepr
    } else {
      println("No urban dictionary data for mention: " + m.mentionId)
      repr
    }
  }


  def annotationsFromFile(m: TacEntityMention, file: File) = {

    val xmlDoc = XML.loadFile(file)

    val entityLinks = xmlDoc \\ "entitylink"
    val linkedMentions = entityLinks.map(e => {
      //println((e \ "name").text)
      val mention = new SimpleEntityMention(m.docId, "", (e \ "mentionId").text, (e \ "name").text, "")
      val candidates = e \\ "candidate" take 10
      val candidateEntities = for (c <- candidates) yield {
        // println(c)
        val id = (c \ "id").text.trim
        val score = (c \ "score").text.trim.toDouble
        val rank = (c \ "rank").text.trim.toInt
        new ScoredWikipediaEntity(id, -1, score, rank)
      }
      val linkedMention = LinkedMention(mention, candidateEntities)
      // println(linkedMention)
      linkedMention
    })


    val tokens = xmlDoc \\ "token"
    val text = tokens.map(t => (t \ "word").text.trim()).mkString(" ")

    val mentions = xmlDoc \\ "mention"

    val neighbors = mentions.map(mention => {
      val span = (mention \ "string").text.trim()
      val tokenStart = (mention \ "TokenBegin").text.trim().toInt
      val tokenEnd = (mention \ "TokenEnd").text.trim().toInt
      val charStart = (mention \ "CharacterOffsetBegin").text.trim().toInt
      val charEnd = (mention \ "CharacterOffsetBegin").text.trim().toInt
      val eType = (mention \ "type").text.trim()
      new NlpXmlNerMention(span, Seq(), -1, false, tokenStart, tokenEnd, charStart, charEnd, eType)

    })

    val titleOption = IdMap.tacId2WikiTitleMap.get(m.nodeId)
    val title = titleOption match {
      case Some(titleOption) => titleOption
      case None => ""
    }
    println("Num neighbors: " + neighbors.size + " num links: " + linkedMentions.size)
    val newMention = TacEntityMention(docId = m.docId
      , entityType = m.entityType
      , mentionId = m.mentionId
      , entityName = m.entityName
      , nodeId = m.nodeId
      , groundTruth = title,
      nerNeighbors = neighbors,
      text = Some(text),
      linkedMentions = linkedMentions
    )

    newMention

  }


  object LinkedMention2XmlRenderer {

    def xml(m:TacEntityMention, links: Seq[ScoredWikipediaEntity], rawRank:Seq[ScoredWikipediaEntity] ) = {
      <root>
        <document>
          <name>
            {m.docId}
          </name>
            <mention>
              <id>
                {m.mentionId}
              </id>
              <string>
                {m.entityName}
              </string>
              <type>
                {m.entityType}
              </type>
              <CharacterOffsetBegin>
                {-1}
              </CharacterOffsetBegin>
              <CharacterOffsetEnd>
                {-1}
              </CharacterOffsetEnd>
              <TokenBegin>
                {-1}
              </TokenBegin>
              <TokenEnd>
                {-1}
              </TokenEnd>
            </mention>
          <kblinks>
            <entitylink>
              <name>
                {m.entityName}
              </name>
              <CharacterOffsetBegin>
                {-1}
              </CharacterOffsetBegin>
              <CharacterOffsetEnd>
                {-1}
              </CharacterOffsetEnd>
              <TokenBegin>
                {-1}
              </TokenBegin>
              <TokenEnd>
                {-1}
              </TokenEnd>{for (c <- links) yield
              <candidate>
                <id>
                  {c.wikipediaTitle}
                </id>
                <rank>
                  {c.rank}
                </rank>
                <score>
                  {c.score}
                </score>
              </candidate>}
            </entitylink>
            {for (c <- rawRank) yield
            <rawcandidate>
              <id>
                {c.wikipediaTitle}
              </id>
              <rank>
                {c.rank}
              </rank>
              <score>
                {c.score}
              </score>
            </rawcandidate>}
          </kblinks>
        </document>
      </root>
    }
  }

}
