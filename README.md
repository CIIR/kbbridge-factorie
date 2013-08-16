kbbridge-factorie
=================

KB Bridge (https://github.com/daltonj/KbBridge) entity linking in the factorie (https://github.com/factorie/) document annotator framework.

KbBridgeEntityLinking is a factorie document annotator class which performs entity linking to Wikipedia using detected entity mentions and the KB Bridge linking system.

The main class in this package to annotate documents is:
cc.factorie.app.nlp.el.LinkingAnnotatorMain

This class takes three arguments:
1) a comma separated list of document identifiers (TAC/TREC)
2) a galago index directory (that includes a corpus file)
3) an output directory where annotations will be written.

By default, the annotated documents are serialized to XML.  They are also printed to the console in a column oriented format.

