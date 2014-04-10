package cc.factorie.app.nlp.el

import java.io.{StringReader, File}
import org.lemurproject.galago.tupleflow.Parameters
import org.lemurproject.galago.core.retrieval.{Retrieval, RetrievalFactory}
import edu.umass.ciir.kbbridge.tac.TacQueryUtil
import cc.factorie.app.nlp.pos._
import edu.umass.ciir.kbbridge.data.{IdMap, ScoredWikipediaEntity, TacEntityMention}
import cc.factorie.app.nlp.ner.{NoEmbeddingsConllStackedChainNer}
import cc.factorie.app.nlp.parse.{OntonotesTransitionBasedParser}
import cc.factorie.app.nlp.{Document, DocumentAnnotatorPipeline, MutableDocumentAnnotatorMap}
import org.xml.sax.InputSource
import scala.xml.XML
import edu.umass.ciir.kbbridge.util.KbBridgeProperties
import edu.umass.ciir.kbbridge.text2kb.{GalagoDoc2WikipediaEntity, QVMLocalTextEntityRepr, KnowledgeBaseCandidateGenerator}
import edu.umass.ciir.kbbridge.RankLibReranker
import edu.umass.ciir.kbbridge.nlp.NlpData.NlpXmlNerMention
import edu.umass.ciir.kbbridge.search.{DocumentBridgeMap, EntityRetrievalWeighting, EntityReprRetrieval}
import cc.factorie.app.nlp.coref.mention.{MentionEntityType, MentionList, MentionType, NerAndPronounMentionFinder}
import org.lemurproject.galago.core.parse.Document.DocumentComponents

/**
 * Created with IntelliJ IDEA.
 * User: jdalton
 * Date: 9/11/13
 * Time: 3:33 PM
 * To change this template use File | Settings | File Templates.
 */
object TacLinkingMain extends App {

  val testMode = true
  var tacQueryIds: Set[String] = args(0).split(",").toSet
  val outputDir: File = new File(args(2))

  val tacQueries: Seq[TacEntityMention] = TacQueryUtil.allQueries()

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

    val nlpSteps = Seq(

      OntonotesForwardPosTagger,
      NoEmbeddingsConllStackedChainNer,
      OntonotesTransitionBasedParser,
      NerAndPronounMentionFinder
    )


    val map = new MutableDocumentAnnotatorMap ++= DocumentAnnotatorPipeline.defaultDocumentAnnotationMap
    for (annotator <- nlpSteps) map += annotator
    val pipeline = DocumentAnnotatorPipeline(map = map.toMap, prereqs = Nil, nlpSteps.flatMap(_.postAttrs))


    for (m <- mentions) {
      val outputFile = new File(outputDir.getAbsolutePath + File.separator + m.mentionId + ".xml")

      if (testMode || !outputFile.exists()) {
        var gDoc = retrieval.getDocument(m.docId, new DocumentComponents(p))

        if (gDoc == null) {
          gDoc = retrieval.getDocument(m.docId.toLowerCase(), new DocumentComponents(p))
        }

        if (gDoc != null) {
          println("Annotating document: " + m.mentionId + " doc:" + m.docId + " name:" + m.entityName)


          //val docXml = XML.loadString(gDoc.text)
          // val newsDoc = Text2FactorieDoc.newswire(b)
          val doc = if (!(m.docId startsWith "bolt")) {
            val parser = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl().newSAXParser()
            val adapter = new scala.xml.parsing.NoBindingFactoryAdapter
            val xmlDoc = adapter.loadXML(new InputSource(new StringReader(gDoc.text)), parser)
            // println(xmlDoc.toString())
            val text = xmlDoc \\ "TEXT"
            // println(text.text)


            val headline = xmlDoc \\ "HEADLINE"
            val doc = Text2FactorieDoc.news(headline, text)
            doc
          } else {
            Bolt2FactorieDoc.text2FactorieDoc(gDoc.text)
          }
          // val doc = new Document(textDoc)


          doc.setName(gDoc.name)
          pipeline.process(doc)

          val text = doc.tokens.map(t => t.string).mkString(" ")

          val newMention = TacEntityMention(docId = m.docId
            , entityType = m.entityType
            , mentionId = m.mentionId
            , entityName = m.entityName
            , nodeId = m.nodeId
            , groundTruth = "",
            nerNeighbors = extractNerNeighborhood(doc),
            text = Some(text) )

          println("Fetching candidates for mention: " + m.mentionId + " d:" + m.docId + " name:" + m.entityName)

          val repr = QVMLocalTextEntityRepr.createEntityRepr(newMention)
          val query = EntityReprRetrieval.buildRawQuery(repr,EntityRetrievalWeighting(0.31, 0.38, 0.0, 0.31), false, 0)
          val numCands =  250
          //println("running query: " + query + " num cands: " + numCands)

          val t0 = System.currentTimeMillis
          val retrievedCands = candidateGenerator.retrieveScoredDocuments(query, numCands)
          val t1 = System.currentTimeMillis()
          val diff = t1-t0
          println(s"Query: $query time: $diff cands: $numCands")

          val candidates = GalagoDoc2WikipediaEntity.galagoResultToWikipediaEntities(retrievedCands)

          val t2 = System.currentTimeMillis
          val rerankedResults = reranker.rerankCandidatesGenerateFeatures(newMention, candidates).toSeq
          val t3 = System.currentTimeMillis()
          val diff2 = t3 - t2
          println(s"Reranking time: $diff2")

          val xml = LinkedMention2XmlRenderer.xml(newMention, rerankedResults, candidates)
          //println(xml.toString)

          println("writing results: " + outputFile.getAbsolutePath)
          XML.save(outputFile.getAbsolutePath, xml, "UTF-8")
        } else {
          println("Unable to process document: " + m.docId)
        }
      }


      //        val xml = new toXml(doc, links)
      //        println(xml.makeDocument.toString)
      //        xml.save(outputFile)
    }

    def extractNerNeighborhood(doc: Document) = {
      val neighbors = doc.attr[MentionList]

      val namedMentions = neighbors.filter(m => {
        val mType = m.attr[MentionType].categoryValue
        (mType equals "NAM")
      })


      val allNers = namedMentions.map(m =>  {
        val eTypeAttr = m.attr[MentionEntityType]
        val eType = if (eTypeAttr != null) {
          eTypeAttr.categoryValue
        } else {
          "UNK"
        }

        val charStart =   m.tokens.head.stringStart
        val charEnd = m.tokens.last.stringEnd
        val tokenStart = m.tokens.head.position
        val tokenEnd = tokenStart + m.tokens.length

        new NlpXmlNerMention(m.string, Seq(), -1, false, tokenStart, tokenEnd, charStart, charEnd, eType)
      }
      )

      allNers.toSeq
    }
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
