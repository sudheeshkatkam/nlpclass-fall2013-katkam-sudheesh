package nlp.a4

import nlpclass.HmmTrainerToImplement
import nlpclass.HiddenMarkovModelToImplement
import nlp.a2.ConditionalProbabilityDistributionWithAllValues
import com.typesafe.scalalogging.log4j.Logging

class AddLambdaSmoothedHmmTrainer[Word, Tag](startWord: Word,
                                             startTag: Tag,
                                             endWord: Word,
                                             endTag: Tag,
                                             lambda: Double) extends HmmTrainerToImplement[Word, Tag] with Logging {

  def train(taggedSentences: Vector[Vector[(Word, Tag)]]): HiddenMarkovModelToImplement[Word, Tag] = {
    val TWPairs = taggedSentences.map {
      sentence =>
        val pairs = (startWord, startTag) +: sentence :+ (endWord, endTag)
        pairs.map { case (word, tag) => (tag, word) }
    }.flatten

    val TTPairs = taggedSentences.map {
      sentence =>
        val pairs = (startWord, startTag) +: sentence :+ (endWord, endTag)
        pairs.sliding(2).toVector.map {
          TTVector =>
            (TTVector(0)._2, TTVector(1)._2)
        }
    }.flatten

    val words = TWPairs.map(_._2).toSet - endWord - startWord
    val tags  = TWPairs.map(_._1).toSet + endTag  - startTag

    //logger.info("words: " + words)
    //logger.info("tags: "  + tags)

    val TWProbs = new ConditionalProbabilityDistributionWithAllValues[Tag, Word](TWPairs, words, lambda)
    val TTProbs = new ConditionalProbabilityDistributionWithAllValues[Tag, Tag] (TTPairs, tags,  lambda)

    //logger.debug(TWProbs.distribution.map { case (tag, dist) => "TAG: " + tag + " DIST: " + dist })
    //logger.debug("TWProbs: " + TWProbs.distribution)
    //logger.debug("TTProbs: " + TTProbs.distribution)

    new HiddenMarkovModel(tags, TWProbs, TTProbs, startWord, startTag, endWord, endTag)
  }

}