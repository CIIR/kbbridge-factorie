package cc.factorie.app.nlp.el

import cc.factorie.app.nlp.phrase.Phrase

import collection.mutable.ArrayBuffer
import edu.umass.ciir.kbbridge.data.ScoredWikipediaEntity
import cc.factorie.app.nlp.coref.Mention

/**
 * User: jdalton
 * Date: 6/12/13
 */
class WikiEntityMentions extends ArrayBuffer[WikiEntity]

case class WikiEntity(phrase:Phrase, entityLinks : Seq[ScoredWikipediaEntity])
