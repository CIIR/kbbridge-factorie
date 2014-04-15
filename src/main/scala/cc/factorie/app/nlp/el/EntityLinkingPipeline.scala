package cc.factorie.app.nlp.el

import cc.factorie.app.nlp._
import org.lemurproject.galago.core.retrieval.{RetrievalFactory, Retrieval}
import java.io.{StringReader, File}
import org.lemurproject.galago.tupleflow.Parameters
import scala.xml.parsing.ConstructingParser
import scala.io.Source
import scala.xml.{Node, NodeSeq, XML}
import org.xml.sax.InputSource
import com.typesafe.scalalogging.slf4j.Logging
import org.lemurproject.galago.core.parse.TagTokenizer
import scala.xml.pull.{EvText, EvElemEnd, EvElemStart, XMLEventReader}


import cc.factorie.app.nlp.pos.{OntonotesForwardPosTagger, OntonotesChainPosTagger}
import cc.factorie.app.nlp.ner.NoEmbeddingsConllStackedChainNer
import cc.factorie.app.nlp.parse.OntonotesTransitionBasedParser
import cc.factorie.variable.CategoricalVar
import cc.factorie.app.nlp.coref.mention.{MentionList, MentionType, NerAndPronounMentionFinder}
import org.lemurproject.galago.core.parse.Document.DocumentComponents

object LinkingAnnotatorMain extends App with Logging {

  println(System.getProperty("file.encoding"))

  val testMode = true
  var docIds: Seq[String] = args(0).split(",").toSeq
  val outputDir: File = new File(args(2))

  //outputDir = new File(args(2))
  if (!outputDir.exists()) {
    outputDir.mkdirs()
  }

  println("docs to annotate: " + docIds.size)

  val p = new Parameters()
  p.set("index", args(1))
  if (testMode || workTodo(docIds)) {
    p.set("terms", true)
    p.set("tags", true)
    val retrieval = RetrievalFactory.instance(p)
    println("Starting annotation:")
    annotateDocs(docIds, retrieval)
  }

  def docFromFile(file: File) : org.lemurproject.galago.core.parse.Document = {

    val source = scala.io.Source.fromFile(file)
    val lines = source.mkString
    source.close()

    val d = new org.lemurproject.galago.core.parse.Document(file.getName(), lines)
    val tt = new TagTokenizer()
    tt.process(d)
    d
  }

  def annotateDocs(docs: Seq[String], retrieval: Retrieval) = {

    val nlpSteps = Seq(

      // Truecasing??
      OntonotesForwardPosTagger,
      NoEmbeddingsConllStackedChainNer,
      OntonotesTransitionBasedParser,
      NerAndPronounMentionFinder,
      KbBridgeEntityLinking       
    )

  //  NER3.ChainNer2FeaturesDomain.freeze()
 //   NER3.ChainNerFeaturesDomain.freeze()

    val map = new MutableDocumentAnnotatorMap ++= DocumentAnnotatorPipeline.defaultDocumentAnnotationMap
    for (annotator <- nlpSteps) map += annotator
    val pipeline = DocumentAnnotatorPipeline(map=map.toMap, prereqs=Nil, nlpSteps.flatMap(_.postAttrs))


    for (docId <- docs) {
      val outputFile = new File(outputDir.getAbsolutePath + File.separator + docId + ".xml")

      if (testMode || !outputFile.exists()) {
        var gDoc = retrieval.getDocument(docId, new DocumentComponents(p))

        if (gDoc == null) {
          gDoc = retrieval.getDocument(docId.toLowerCase(), new DocumentComponents(p))
        }

        if (gDoc != null) {
          println("Annotating document: " + docId )


          //val docXml = XML.loadString(gDoc.text)
          // val newsDoc = Text2FactorieDoc.newswire(b)
          val doc = if (!(docId startsWith "bolt"))  {
          val parser = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl().newSAXParser()
          val adapter = new scala.xml.parsing.NoBindingFactoryAdapter
          val xmlDoc = adapter.loadXML(new InputSource(new StringReader(gDoc.text)), parser)
          // println(xmlDoc.toString())
          val text = xmlDoc \\ "TEXT"
          // println(text.text)


          val headline = xmlDoc \\ "HEADLINE"
          val doc = Text2FactorieDoc.news(headline, text)
            doc
          }  else {
            Bolt2FactorieDoc.text2FactorieDoc(gDoc.text)
          }
          // val doc = new Document(textDoc)


          doc.setName(gDoc.name)
          pipeline.process(doc)

          println("Processed %d tokens.".format(doc.tokenCount))
          println(doc.owplString(nlpSteps.map(p => p.tokenAnnotationString(_))))

          val xml = Document2XmlRenderer.xml(doc)
          //println(xml.toString)

          XML.save(outputFile.getAbsolutePath, xml, "UTF-8")
        } else {
          println("Unable to process document: " + docId)
        }
      }


      //        val xml = new toXml(doc, links)
      //        println(xml.makeDocument.toString)
      //        xml.save(outputFile)
    }
  }


  def workTodo(docs: Seq[String]): Boolean = {
    var workTodo = false
    for (docId <- docs) {
      val outputFile = new File(outputDir.getAbsolutePath + File.separator + docId + ".xml")
      if (!outputFile.exists()) {
        workTodo = true
      }
    }
    workTodo
  }

}

class Paragraph(val document:Document, val stringStart:Int, val stringEnd:Int) extends Section

object Bolt2FactorieDoc {

  var offset = 0
  var start = 0
  var inside : Boolean = false

  def text2FactorieDoc(text:String) : Document = {
    val d = new Document(text)
    val src = Source.fromString(text)
    val er = new XMLEventReader(src)
    parse(er,d)
    d
  }

  def parse(xml: XMLEventReader, d : Document) {
    def loop(currNode: List[String]) {
      if (xml.hasNext) {
        xml.next match {
          case EvElemStart(_, label, a, _) =>
            //println("Start element: " + label)
            if(label == "doc") {
              val m = a.get("id").getOrElse(Seq[Node]()).head.toString()
              d.setName(a.get("id").getOrElse(Seq[Node]()).head.toString())
            }
            loop(label :: currNode)
          case EvElemEnd(_, label) =>
            //println("End element: " + label)
            if(currNode.head == ("post"))
              addSection(d)
            loop(currNode.tail)
          case EvText(text) =>
            addSection(text, currNode, d)
            loop(currNode)
          case _ => loop(currNode)
        }
      }
    }
    start = 0
    offset = 0
    loop(List.empty)
  }


  def addSection(text : String, currNode : List[String], d : Document) {
    if(currNode.isEmpty) return
    if(d.string.indexOf(text, offset) == -1) {
      println(d.string + " cannot find: " + text)
      println(text)
      println("Offset: " + offset)
      //println("After offset: " + d.string.substring(offset))
      offset = d.string.indexOf(text)
    }
    if(currNode.head == "post") {
      if(!inside) {
        start = d.string.indexOf(text, offset)
        inside = true
      }
      offset = d.string.indexOf(text, offset) + text.length
    }
    if(currNode.head == "quote") {
      if(inside) {
        if(offset > start && d.string.substring(start,offset).trim.length > 1) d += new Paragraph(d, start, offset)
        start = d.string.indexOf(text, offset) + text.length
        inside = false
      } else {
        start = d.string.indexOf(text, offset) + text.length
      }
      offset = d.string.indexOf(text, offset) + text.length
    }
    if(currNode.head == "a" && !currNode.contains("quote")) {
      if(!inside) {
        start = d.string.indexOf(text, offset)
        inside = true
      }
      offset = d.string.indexOf(text, offset) + text.length + 4
    }
  }
  def addSection(d : Document) {
    inside = false
    if(offset > start && d.string.substring(start,offset).trim.length > 1) d += new Paragraph(d, start, offset)
  }

}

object Text2FactorieDoc {

  def news(headline: NodeSeq, text: NodeSeq): Document = {
    val paragraphs = (text \\ "p")
    val headlineText = headline.text.replace("\n", " ")

    val mText = if (paragraphs.size > 0) {
      paragraphs.map(node => node.text.trim.replace("\n", " ")).mkString("\n\n")
    } else {
      text.text.replace("\n", " ")
    }
    // punctuation hack to ensure that the end of document gets detected.
    val cleanLayout = removeLayout(headlineText ++ ". \n\n" + mText) + "."
    // println("TEXT:\n" + cleanLayout)
    new Document(cleanLayout)
  }

  def ensurePunc(string: String): String = {
    if (string.trim.split("\n").last.matches(".*[\\.?!]$")) string.trim else string.trim + "."
  }

  def removeLayout(string: String): String = {
    string.replaceAll("====*|----*|\\*\\*\\*\\**|XXXX*", ".").replaceAll("(\\.( \n|\n |\n| )?)(\\.( \n|\n |\n| )?)+", ".")
  }

  def unescape(string: String): String = {
    val s = "<doc>" + string.replaceAll("&(?![a-z]{2,4};)", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;") + "</doc>"
    val d = ConstructingParser.fromSource(Source.fromString(s), preserveWS = true).document()
    d(0).text
  }

}





object Document2XmlRenderer {

//  def sgmlString(doc: Document): String = {
//    val buf = new StringBuffer
//    for (section <- doc.sections; token <- section.tokens) {
//      if (token.isSentenceStart) buf.append("<sentence>")
//      token.startsSpans.foreach(span => buf.append("<" + span.name + ">"))
//      buf.append(token.string)
//      token.endsSpans.foreach(span => buf.append("</" + span.name + ">"))
//      if (token.isSentenceEnd) buf.append("</sentence>")
//      buf.append(" ")
//    }
//    buf.toString
//  }

  def xml(doc: Document) = {
    <root>
      <document>
        <name>
          {doc.name}
        </name>
        <tokens>
          {for (section <- doc.sections; token <- section.tokens) yield
          <token id={(token.position + 1).toString}>
            <word>
              {token.string}
            </word>
            <lemma>
              {token.string}
            </lemma>
            <POS>
              {getAttr(token, OntonotesForwardPosTagger.tokenAnnotationString(_))}
            </POS>
            <CharacterOffsetBegin>
              {token.stringStart}
            </CharacterOffsetBegin>
            <CharacterOffsetEnd>
              {token.stringEnd}
            </CharacterOffsetEnd>
            <NER>
              {getAttr(token, NoEmbeddingsConllStackedChainNer.tokenAnnotationString(_))}
            </NER>
            <PARSE>
              {getAttr(token, OntonotesTransitionBasedParser.tokenAnnotationString(_))}
            </PARSE>
            <StartSentence>
              {token.isSentenceStart}
            </StartSentence>
          </token>}
        </tokens>
        <mentions>
          {for (m <- doc.attr[MentionList]) yield
          <mention>
            <string>
              {m.string}
            </string>
            <type>
              {m.attr[MentionType].categoryValue}
            </type>
            <CharacterOffsetBegin>
              {m.tokens.head.stringStart}
            </CharacterOffsetBegin>
            <CharacterOffsetEnd>
              {m.tokens.last.stringEnd}
            </CharacterOffsetEnd>
            <TokenBegin>
              {m.tokens.head.position}
            </TokenBegin>
            <TokenEnd>
              {m.tokens.head.position + m.tokens.length}
            </TokenEnd>
          </mention>}
        </mentions>{if (doc.attr[WikiEntityMentions] != null) {
        <kblinks>
          {for (linkedMention <- doc.attr[WikiEntityMentions]) yield
          <entitylink>
            <name>
              {linkedMention.mention.string}
            </name>
            <CharacterOffsetBegin>
              {linkedMention.mention.tokens.head.stringStart}
            </CharacterOffsetBegin>
            <CharacterOffsetEnd>
              {linkedMention.mention.tokens.last.stringEnd}
            </CharacterOffsetEnd>
            <TokenBegin>
              {linkedMention.mention.tokens.head.position}
            </TokenBegin>
            <TokenEnd>
              {linkedMention.mention.tokens.head.position + linkedMention.mention.tokens.length}
            </TokenEnd>{for (c <- linkedMention.entityLinks) yield
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
          </entitylink>}
        </kblinks>
      }}
      </document>
    </root>
  }

  def getAttr(token: Token, af: (Token) => Any): String = {
    af(token) match {
      case cv: CategoricalVar[String @unchecked] => cv.categoryValue.toString
      case null => ""
      case v: Any => v.toString
    }
  }


}

