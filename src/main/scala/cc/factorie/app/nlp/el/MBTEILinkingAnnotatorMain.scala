package cc.factorie.app.nlp.el

import java.io.File

import cc.factorie.app.nlp._
import cc.factorie.app.nlp.ner.NoEmbeddingsConllStackedChainNer
import cc.factorie.app.nlp.parse.OntonotesTransitionBasedParser
import cc.factorie.app.nlp.phrase.NPChunkMentionFinder
import cc.factorie.app.nlp.pos.OntonotesForwardPosTagger
import ciir.proteus.parse.MBTEIPageParser
import com.typesafe.scalalogging.slf4j.Logging
import org.lemurproject.galago.core.types.DocumentSplit
import org.lemurproject.galago.utility.Parameters

import scala.xml.XML


// Params:
// comma seperated list of doc IDs - including extension (.mbtei or .mbtei.gz)
// path to docs
// ouotput path
object MBTEILinkingAnnotatorMain extends App with Logging {

  val testMode = false
  var docIds: Seq[String] = args(0).split(",").toSeq
  var inputDir = args(1)
  val outputDir: File = new File(args(2))

  //outputDir = new File(args(2))
  if (!outputDir.exists()) {
    outputDir.mkdirs()
  }

  println("docs to annotate: " + docIds.size)

  annotateDocs(docIds, inputDir)


  def annotateDocs(docs: Seq[String], baseDirectory: String) = {

    val nlpSteps = Seq(

      OntonotesForwardPosTagger,
      NoEmbeddingsConllStackedChainNer,
      OntonotesTransitionBasedParser,
      NPChunkMentionFinder,
      KbBridgeEntityLinking
    )


    val map = new MutableDocumentAnnotatorMap ++= DocumentAnnotatorPipeline.defaultDocumentAnnotationMap
    for (annotator <- nlpSteps) map += annotator
    val pipeline = DocumentAnnotatorPipeline(map = map.toMap, prereqs = Nil, nlpSteps.flatMap(_.postAttrs))


    for (docId <- docs) {
      var pageno = 0
      //val outputFile = new File(outputDir.getAbsolutePath + File.separator + docId + "_" + pageno + ".xml")

      // TODO parse the doc
      // the parser can handle gzipped or decompressed files
      val docFile = new File(baseDirectory + File.separator + docId)

      val split = new DocumentSplit();
      split.fileName = docFile.getAbsolutePath();
      val parser = new MBTEIPageParser(split, new Parameters())
      println("Loading document: " + docFile.getAbsolutePath)
      //if ((testMode || !outputFile.exists()) && docFile.exists()) {
      if (testMode || docFile.exists()) {

        var page = parser.nextDocument()
        while (page != null) {

          val outputFile = new File(outputDir.getAbsolutePath + File.separator + docId + "_" + pageno + ".xml")
          pageno = pageno + 1
          val doc = new Document(page.text)
          doc.setName(docId)
          pipeline.process(doc)


          println("Processed %d tokens.".format(doc.tokenCount))
          println(doc.owplString(nlpSteps.map(p => p.tokenAnnotationString(_))))

          val xml = Document2XmlRenderer.xml(doc)
          //println(xml.toString)

          XML.save(outputFile.getAbsolutePath, xml, "UTF-8")
          page = parser.nextDocument()
        } // end while we still have pages

      } else {
        println("Unable to process document: " + docId)
      }
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

  def readDocument(doc: String, baseDir: String): String = {
    val src = scala.io.Source.fromFile(baseDir + File.separator + doc, "UTF-8")
    println("Loading document from file: " + baseDir + File.separator + doc)
    val lines = src.getLines().toIndexedSeq
    src.close()
    lines.mkString("\n")
  }

}






