package nlp.a4

import nlpclass.HiddenMarkovModelToImplement
import nlpclass.ConditionalProbabilityDistributionToImplement
import scala.math
import com.typesafe.scalalogging.log4j.Logging

class HiddenMarkovModel[Word, Tag](tags: Set[Tag],
                                   TWProbs: ConditionalProbabilityDistributionToImplement[Tag, Word],
                                   TTProbs: ConditionalProbabilityDistributionToImplement[Tag, Tag],
                                   startWord: Word, startTag: Tag,
                                   endWord: Word, endTag: Tag) extends HiddenMarkovModelToImplement[Word, Tag] with Logging {

  def sentenceProb(sentence: Vector[(Word, Tag)]): Double = {
    val pairs = (startWord, startTag) +: sentence :+ (endWord, endTag)

    val emissions = pairs.map { case (tag, word) => TWProbs(tag, word) }
    val transitions = pairs.sliding(2).toVector.map { TTVector => TTProbs(TTVector(1)._2, TTVector(0)._2) }
    
    logger.debug("emissions: "   + emissions)
    logger.debug("transitions: " + transitions)

    (transitions ++ emissions).foldLeft(0.0) { (acc, prob) => acc + math.log(prob) }
  }

  def tagSentence(sentence: Vector[Word]): Vector[Tag] = {
    val sequence = startWord +: sentence :+ endWord

    Viterbi[Word, Tag](tags, startTag, endTag, sequence, TWProbs, TTProbs).drop(1).dropRight(1)
  }

}