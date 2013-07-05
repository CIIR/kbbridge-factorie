package cc.factorie.app.nlp.el

import collection.mutable.ArrayBuffer
import cc.factorie.app.nlp.mention.Mention
import edu.umass.ciir.kbbridge.data.ScoredWikipediaEntity

/**
 * User: jdalton
 * Date: 6/12/13
 */
class WikiEntityMentions extends ArrayBuffer[WikiEntity]

case class WikiEntity(val mention:Mention, val entityLinks : Seq[ScoredWikipediaEntity])
