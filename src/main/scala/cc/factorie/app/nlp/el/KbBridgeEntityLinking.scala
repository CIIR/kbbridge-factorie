package cc.factorie.app.nlp.el

import cc.factorie.app.nlp.{Sentence, Token, Document, DocumentAnnotator}
import cc.factorie.app.nlp.ner.{ChainNerLabel, BilouConllNerLabel}
import cc.factorie.BP
import scala.Predef._
import cc.factorie.app.nlp.mention.{MentionType, Mention, MentionList}
import edu.umass.ciir.kbbridge.text2kb.KnowledgeBaseCandidateGenerator
import edu.umass.ciir.kbbridge.RankLibReranker
import edu.umass.ciir.kbbridge.util.{ConfInfo, KbBridgeProperties}
import edu.umass.ciir.kbbridge.data.SimpleEntityMention
import cc.factorie.app.nlp.coref.EntityType
import edu.umass.ciir.kbbridge.nlp.NlpData.NlpXmlNerMention

/**
 * User: jdalton
 * Date: 6/12/13
 */



object KbBridgeEntityLinking extends DocumentAnnotator {

  lazy val candidateGenerator = KnowledgeBaseCandidateGenerator()
  val reranker = new RankLibReranker(KbBridgeProperties.rankerModelFile)

  override def tokenAnnotationString(token:Token): String = {

    val matchingingMention = token.document.attr[WikiEntityMentions].filter(e => e.mention.span.contains(token))

    matchingingMention match {
      case wi:Seq[WikiEntity] if wi.length > 0 => wi.map(m => (m.entityLinks take 3).map(e => e.wikipediaTitle + ":" + "%3f".format(e.score)).mkString(",")).mkString(",")
      case _ => " "
    }
//      m.attr[MentionType].categoryValue+":"+m.span.indexOf(token)).mkString(","); case _ => "_" }
//
//
//    val entities = token.attr[WikiEntityMentions]
//    val entString = if (entities != null) {
//      entities.head.entityLinks.head.wikipediaTitle
//    } else {
//      "NIL"
//    }
//    entString
  }

  def prereqAttrs: Iterable[Class[_]] = List(classOf[MentionList])
  def postAttrs: Iterable[Class[_]] = List(classOf[WikiEntityMentions])
  def process1(doc:Document): Document = {

    val mentionsToReturn = new WikiEntityMentions

    val neighbors = doc.attr[cc.factorie.app.nlp.mention.MentionList].filter(m => {
      val mType = m.attr[MentionType].categoryValue
      mType equals "NAM"
    })


    val allNers = neighbors.map(m =>  {
      val eTypeAttr = m.attr[EntityType]
      val eType = if (eTypeAttr != null) {
        eTypeAttr.categoryValue
      } else {
        "UNK"
      }

      val start =   m.span.tokens.head.stringStart
      val end = m.span.tokens.last.stringEnd
      new NlpXmlNerMention(m.span.string, Seq(), -1, false, (start), end, start,(end), eType)
    }
    )

    val text = doc.tokens.map(t => t.string).mkString(" ")

    mentionsToReturn ++= neighbors.map(m => linkEntity(m, doc, text, allNers))

    doc.attr +=  mentionsToReturn

    //doc.attr[WikiEntityMentions]
    doc
  }

  def linkEntity(mention : Mention, d: Document, text:String, allNers: Seq[NlpXmlNerMention]) : WikiEntity = {

    val eTypeAttr = mention.attr[EntityType]
    val eType = if (eTypeAttr != null) {
      eTypeAttr.categoryValue
    } else {
      "UNK"
    }

    val start =   mention.span.tokens.head.stringStart
    val end = mention.span.tokens.last.stringEnd

    val bridgeMention = new SimpleEntityMention(d.name, eType, (d.name +"_s"+ (start) +"-" +(end)),
      entityName=mention.span.string, fullText=text, Seq(), nerNeighbors=allNers, "")

    println("Fetching candidates for mention: " + bridgeMention.mentionId + " d:" + bridgeMention.docId + " name:" + bridgeMention.entityName)
    val candidates = candidateGenerator.retrieveCandidates(bridgeMention, ConfInfo.maxEntityCandidates)
    val rerankedResults = reranker.rerankCandidatesGenerateFeatures(bridgeMention, candidates).toSeq
    WikiEntity(mention, rerankedResults)
  }
}
