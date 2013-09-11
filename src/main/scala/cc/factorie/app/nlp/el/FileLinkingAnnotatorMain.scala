package cc.factorie.app.nlp.el

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.pos.POS1
import java.io.{ File}
import scala.xml.XML
import com.typesafe.scalalogging.slf4j.Logging
import org.lemurproject.galago.core.parse.TagTokenizer

import cc.factorie.app.nlp.ner.NER1
import cc.factorie.app.nlp.parse.DepParser1
import cc.factorie.app.nlp.mention.{NerAndPronounMentionFinder, ParseBasedMentionFinding}

object FileLinkingAnnotatorMain extends App with Logging {

  val testMode = false
  var docIds: Seq[String] = args(0).split(",").toSeq
  val outputDir: File = new File(args(2))

  //outputDir = new File(args(2))
  if (!outputDir.exists()) {
    outputDir.mkdirs()
  }

  println("docs to annotate: " + docIds.size)

  annotateDocs(docIds, args(1))


  def docFromFile(file: File) : org.lemurproject.galago.core.parse.Document = {

    val source = scala.io.Source.fromFile(file)
    val lines = source.mkString
    source.close()

    val d = new org.lemurproject.galago.core.parse.Document(file.getName(), lines)
    val tt = new TagTokenizer()
    tt.process(d)
    d
  }

  def annotateDocs(docs: Seq[String], baseDirectory:String) = {

    val nlpSteps = Seq(

      // Truecasing??
      POS1,
      // LemmaAnnotator,
      NER1,
      //FactorieNERComponent,
      DepParser1,
      NerAndPronounMentionFinder,
      KbBridgeEntityLinking
    )


    val map = new MutableDocumentAnnotatorMap ++= DocumentAnnotatorPipeline.defaultDocumentAnnotationMap
    for (annotator <- nlpSteps) map += annotator
    val pipeline = DocumentAnnotatorPipeline(map=map.toMap, prereqs=Nil, nlpSteps.flatMap(_.postAttrs))


    for (docId <- docs) {
      val outputFile = new File(outputDir.getAbsolutePath + File.separator + docId + ".xml")
      val docFile = new File(baseDirectory + File.separator + docId)
      println("Loading document: " + docFile.getAbsolutePath)

      if ((testMode || !outputFile.exists()) && docFile.exists()) {
          val text = readDocument(docId, baseDirectory)
          val doc = new Document(text)
          doc.setName(docId)
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

  def readDocument(doc:String, baseDir:String) : String = {
    val src = scala.io.Source.fromFile(baseDir + File.separator + doc, "UTF-8")
    println("Loading document from file: " + baseDir + File.separator + doc)
    val lines = src.getLines().toIndexedSeq
    src.close()
    lines.mkString("\n")
  }

}









