package nlp.a4

import nlpclass.HmmTrainerToImplement
import nlpclass.HiddenMarkovModelToImplement
import nlp.a1.ConditionalProbabilityDistribution
import com.typesafe.scalalogging.log4j.Logging

class UnsmoothedHmmTrainer[Word, Tag](startWord: Word,
                                      startTag: Tag,
                                      endWord: Word,
                                      endTag: Tag) extends HmmTrainerToImplement[Word, Tag] with Logging{

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

    val tags = taggedSentences.flatten.map(_._2).toSet

    val TWProbs = new ConditionalProbabilityDistribution[Tag, Word](TWPairs)
    val TTProbs = new ConditionalProbabilityDistribution[Tag, Tag](TTPairs)
    
    logger.debug("TWProbs: " + TWProbs.distribution)
    logger.debug("TTProbs: " + TTProbs.distribution)

    new HiddenMarkovModel(tags, TWProbs, TTProbs, startWord, startTag, endWord, endTag)
  }

  //val data = Vector(Vector(("the","D"),("man","N"),("walks","V"),("the","D"),("dog","N")),Vector(("the","D"),("dog","N"),("runs","V")),Vector(("the","D"),("dog","N"),("walks","V")),Vector(("the","D"),("man","N"),("walks","V")),Vector(("a","D"),("man","N"),("saw","V"),("the","D"),("dog","N")),Vector(("the","D"),("cat","N"),("walks","V")))

}