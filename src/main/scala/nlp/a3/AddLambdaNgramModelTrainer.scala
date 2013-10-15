package nlp.a3

import nlpclass.NgramModelTrainerToImplement
import nlpclass.NgramModelToImplement
import nlp.a2.ConditionalProbabilityDistributionWithAllValues
import nlp.a1.ProbabilityDistribution

class AddLambdaNgramModelTrainer(n: Int, lambda: Double) extends NgramModelTrainerToImplement {

  override def train(tokenizedSentences: Vector[Vector[String]]): NgramModelToImplement = {
    assert(n >= 0)
    if (n > 1) {
      val data = tokenizedSentences.map {
        sentenceTokens =>
          val sentence = Vector.fill(n - 1)("<S>") ++ sentenceTokens :+ "<E>"
          sentence.zipWithIndex.drop(n - 1).map {
            case (x, i) => (sentence.slice(i - (n - 1), i), x)
          }
      }.flatten

      val vocab = data.map(_._2).toSet
      new NgramModel(n, new ConditionalProbabilityDistributionWithAllValues[Vector[String], String](data, vocab, lambda))
    } else {
      val words = tokenizedSentences.flatten
      val data = Vector.fill(words.size)(Vector.empty).zip(words)
      val vocab = words.toSet
      new NgramModel(n, new ConditionalProbabilityDistributionWithAllValues[Vector[String], String](data, vocab, lambda))
    }
  }

}