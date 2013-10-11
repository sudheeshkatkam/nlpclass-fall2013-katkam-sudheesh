package nlp.a3

import nlpclass.NgramModelToImplement
import nlp.a1.ConditionalProbabilityDistribution

class InterpolatedNgramModel(val n: Int, ngramProbs: ConditionalProbabilityDistribution[Vector[String], String]) extends NgramModelToImplement {

  override def sentenceProb(sentenceTokens: Vector[String]): Double = {
    ???
  }

  override def generate(): Vector[String] = {
    ???
  }

}