package cc.factorie.app.nlp.el

import cc.factorie.app.nlp.{pos, Document, DocumentAnnotator}
import cc.factorie.app.nlp.segment.ClearSegmenter
import cc.factorie.app.nlp.pos.POS1
import org.lemurproject.galago.core.retrieval.{RetrievalFactory, Retrieval}
import java.io.{StringReader, File}
import org.lemurproject.galago.tupleflow.Parameters
import scala.xml.parsing.ConstructingParser
import scala.io.Source
import scala.xml.{PrettyPrinter, NodeSeq, XML}
import scala.collection.mutable.{HashMap, ArrayBuffer}
import org.xml.sax.InputSource

//import com.googlecode.clearnlp.morphology.EnglishMPAnalyzer
import cc.factorie.app.nlp.ner.{NER1, NER2}
import cc.factorie.app.nlp.parse.DepParser2
import cc.factorie.app.nlp.mention.ParseBasedMentionFinding

/**
 * Created with IntelliJ IDEA.
 * User: jdalton
 * Date: 7/2/13
 * Time: 1:57 PM
 * To change this template use File | Settings | File Templates.
 */
object EntityLinkingPipeline  extends DocumentAnnotator {
  def prereqAttrs: Iterable[Class[_]] =  null
  def postAttrs: Iterable[Class[_]] =  null

  lazy val nlpSteps = Seq(

    ClearSegmenter,
   // Truecasing??
    pos.POS1,
   // LemmaAnnotator,
   // NER1,
    DepParser2,
    ParseBasedMentionFinding,
    KbBridgeEntityLinking
  )

  def init() {
    // load models!
  }

  def process1(doc:Document):Document = {

    // run the NLP pipeline over the doc
    for (s <- nlpSteps) {
      println("Starting: " + s.getClass)
      s.process1(doc)
    }

    doc

  }

}



//object LemmaAnnotator extends DocumentAnnotator {
//
//  import cc.factorie.app.nlp.lemma.TokenLemma
//  import com.googlecode.clearnlp.morphology.EnglishMPAnalyzer
//
//  val model = ""
//  lazy val analyzer = new EnglishMPAnalyzer(model)
//
//  def process1(doc: Document): Document = {
//    assert(doc.tokens.head.posLabel ne null)
//    for (t <- doc.tokens) {
//      val posLab = t.posLabel
//      assert(posLab != null)
//      val lemma = analyzer.getLemma(t.string, posLab.categoryValue.toUpperCase())
//      if(t.attr[TokenLemma] ne null)
//        t.attr +=  new TokenLemma(t, lemma)  //todo: is this properly overriding the existing one?
//      else
//        t.attr += new TokenLemma(t, lemma)
//
//    }
//    doc
//  }
//
//}


object DocumentAnnotatorMain extends App {


  val p = new Parameters()

  var docs: Seq[String] = List.empty[String]
  val outputDir: File = new File("/usr/aubury/scratch2/jdalton/edir/robust04-ent-ann1/")

  if (args.length > 0) {
    val docList = args(0).split(",").toSeq
    p.set("index", args(1))

    //outputDir = new File(args(2))
    if (!outputDir.exists()) {
      outputDir.mkdirs()
    }

    docs = docList


    println("docs to annotate: " + docs.size)

    val nlpPipeline = EntityLinkingPipeline

    if (workTodo(docs)) {
      p.set("terms", true)
      p.set("tags", true)
      val retrieval = RetrievalFactory.instance(p)

      nlpPipeline.init()

      annotateDocs(docs, retrieval)
    }

    def annotateDocs(docs: Seq[String], retrieval: Retrieval) = {
      var numAnnotated = 0
      for (docId <- docs) {
        val outputFile = new File(outputDir.getAbsolutePath + File.separator + docId + ".xml")

        if (!outputFile.exists()) {
          val gDoc = retrieval.getDocument(docId, p)

          if (gDoc != null) {
            println("Annotating document: " + docId + " numAnnotated:" + numAnnotated)


            //val docXml = XML.loadString(gDoc.text)
            val b = new BadXmlReader(gDoc.text)
           // val newsDoc = Text2FactorieDoc.newswire(b)

            val parser = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl().newSAXParser()
            val adapter = new scala.xml.parsing.NoBindingFactoryAdapter
            val xmlDoc = adapter.loadXML(new InputSource(new StringReader(gDoc.text)), parser)
           // println(xmlDoc.toString())
            val text =  xmlDoc \\ "TEXT"
           // println(text.text)


            val headline =  xmlDoc \\ "HEADLINE"
            val doc = Text2FactorieDoc.newswire(headline, text)

            // val doc = new Document(textDoc)

            doc.setName(gDoc.name)
            nlpPipeline.process1(doc)
            println("Processed %d tokens.".format(doc.tokenCount))
            println(doc.owplString(EntityLinkingPipeline.nlpSteps.map(p => p.tokenAnnotationString(_))))
          }
        }

      }
    }


    def workTodo(docs: Seq[String]) : Boolean = {
      var workTodo = false
      for (docId <- docs) {
        val outputFile = new File(outputDir.getAbsolutePath + File.separator + docId + ".xml")
        if(!outputFile.exists()) {
          workTodo = true
        }
      }
      workTodo
    }
  }
}

object Text2FactorieDoc {

  def newswire(headline: NodeSeq, text: NodeSeq) : Document =  {
    val paragraphs = (text \\ "p")
    val headlineText = headline.text.replace("\n", " ")

    val mText =  if (paragraphs.size > 0) {
      paragraphs.map(node => node.text.trim.replace("\n", " ")).mkString("\n\n")
      } else {
      text.text.replace("\n", " ")
    }
    val cleanLayout = removeLayout(headlineText ++ "\n\n" + mText)
    new Document(cleanLayout)
  }

  def ensurePunc(string: String): String = {
    if (string.trim.split("\n").last.matches(".*[\\.?!]$")) string.trim else string.trim + "."
  }

  def removeLayout(string: String): String = {
    string.replaceAll("====*|----*|\\*\\*\\*\\**|XXXX*", ".").replaceAll("(\\.( \n|\n |\n| )?)(\\.( \n|\n |\n| )?)+", ".")
  }

  def unescape(string : String) : String = {
    val s = "<doc>" + string.replaceAll("&(?![a-z]{2,4};)","&amp;").replaceAll("<", "&lt;").replaceAll(">","&gt;") + "</doc>"
    val d = ConstructingParser.fromSource(Source.fromString(s), preserveWS = true).document()
    d(0).text
  }

}


class BadXmlReader(var source : String) {
  def \(s : String) : ArrayBuffer[BNode] = head.nodes.head._2.head \ s
  def attributes(s : String) = head.nodes.head._2.head.attributes(s)
  def emptyAttr = head.nodes.head._2.head.attributes.isEmpty
  def $(s : String) : ArrayBuffer[BNode] = head.nodes(s)
  val head = new BNode("TOP", source, source, 0, source.length, source, source.length, null)
}
class BNode(val name : String, val innerText : String, val outerText : String, val startInd : Int, val endInd : Int, val source : String, val tend : Int, val parent : BNode) {
  val tag = "<[A-Za-z ]*( [A-Za-z_]*=[A-Za-z\"&;\\-/\\[\\]\\.:0-9_@,\\(\\) ]*)*>".r
  val tagNameReg = "<[A-Za-z]*".r
  val attributesReg = "[A-Za-z_]*=\"[A-Za-z &;\\-/\\[\\]\\.:0-9_@,\\(\\) ]*\"".r
  val nodes : HashMap[String,ArrayBuffer[BNode]] = new HashMap[String,ArrayBuffer[BNode]]()
  val attributes : HashMap[String, String] = new HashMap[String, String]()
  val tags = tag.findAllIn(innerText).matchData
  var lastEnd : Int = 0
  val q = "<QUOTE PREVIOUSPOST=\"".r
  val q2 = "\">"
  val firstQuote = q.findFirstMatchIn(innerText).getOrElse(null)
  val endFirstQuote = if(firstQuote == null) endInd else innerText.indexOf(q2, firstQuote.start) + startInd + 2

  def quotableEnd : Int = if(firstQuote == null) endInd else firstQuote.start + startInd
  def quotableEndEnd : Int = endFirstQuote

  def \(s : String) : ArrayBuffer[BNode] = if(nodes.contains(s)) nodes(s) else ArrayBuffer[BNode]()

  def afterTextStart : Int = this.tend
  def afterTextEnd : Int = this.parent.endInd

  def afterText : String = source.substring(afterTextStart, afterTextEnd)

  for(t <- tags) {
    val start = t.start + startInd
    val end = t.end + startInd
    if(start >= lastEnd) {
      val s = source.substring(start, end)
      val attributes = attributesReg.findAllIn(s)
      val tagName = tagNameReg.findFirstIn(s).getOrElse("").substring(1)
      var endStart = 0
      var endEnd = 0
      var endS = ""
      if(s.endsWith("/>")) {
        endStart = end
        endEnd = end
        endS = tagName
      } else {
        val endRegex = ("</" + tagName + ">").r
        val endTag = endRegex.findFirstMatchIn(source.substring(end)).getOrElse(null)
        endStart = endTag.start + end
        endEnd = endTag.end + end
        endS = source.substring(endStart, endEnd)
      }
      lastEnd = endEnd
      val node = new BNode(tagName, source.substring(end,endStart), source.substring(start,endEnd), end,endStart,source, endEnd, this)

      for(a <- attributes) {
        val split = a.split("=")
        val key = split(0)
        val value = split(1).substring(1,split(1).length-1)
        node.attributes(key) = value
      }
      if(nodes.contains(tagName)) nodes(tagName) += node
      else nodes(tagName) = ArrayBuffer(node)
    }
  }
}

