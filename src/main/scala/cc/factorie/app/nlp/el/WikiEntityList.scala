package cc.factorie.app.nlp.el

import collection.mutable.ArrayBuffer
import edu.umass.ciir.kbbridge.data.ScoredWikipediaEntity
import cc.factorie.app.nlp.coref.mention.Mention

/**
 * User: jdalton
 * Date: 6/12/13
 */
class WikiEntityMentions extends ArrayBuffer[WikiEntity]

case class WikiEntity(val mention:Mention, val entityLinks : Seq[ScoredWikipediaEntity])
