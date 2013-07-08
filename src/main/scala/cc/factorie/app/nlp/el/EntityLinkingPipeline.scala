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

object DocumentAnnotatorMain extends App {

  val nlpSteps = Seq(

    ClearSegmenter,
    // Truecasing??
    POS1,
    // LemmaAnnotator,
    // NER1,
    DepParser2,
    ParseBasedMentionFinding,
    KbBridgeEntityLinking
  )

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


    if (workTodo(docs)) {
      p.set("terms", true)
      p.set("tags", true)
      val retrieval = RetrievalFactory.instance(p)

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
           // val newsDoc = Text2FactorieDoc.newswire(b)

            val parser = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl().newSAXParser()
            val adapter = new scala.xml.parsing.NoBindingFactoryAdapter
            val xmlDoc = adapter.loadXML(new InputSource(new StringReader(gDoc.text)), parser)
           // println(xmlDoc.toString())
            val text =  xmlDoc \\ "TEXT"
           // println(text.text)


            val headline =  xmlDoc \\ "HEADLINE"
            val doc = Text2FactorieDoc.news(headline, text)

            // val doc = new Document(textDoc)

            doc.setName(gDoc.name)
            for (step <- nlpSteps) {
              step.process1(doc)
            }
            println("Processed %d tokens.".format(doc.tokenCount))
            println(doc.owplString(nlpSteps.map(p => p.tokenAnnotationString(_))))
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

  def news(headline: NodeSeq, text: NodeSeq) : Document =  {
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

