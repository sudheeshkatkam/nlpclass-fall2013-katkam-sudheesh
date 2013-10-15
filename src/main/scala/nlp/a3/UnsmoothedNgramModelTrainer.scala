package nlp.a3

import nlpclass.NgramModelTrainerToImplement
import nlpclass.NgramModelToImplement
import nlp.a1.ConditionalProbabilityDistribution

class UnsmoothedNgramModelTrainer(n: Int) extends NgramModelTrainerToImplement {

  override def train(tokenizedSentences: Vector[Vector[String]]): NgramModelToImplement = {
    //assert(n >= 0)
    if (n > 1) {
      val data = tokenizedSentences.map { sentenceTokens =>
        val sentence = Vector.fill(n - 1)("<S>") ++ sentenceTokens :+ "<E>"
        sentence.zipWithIndex.drop(n - 1).map {
          case (x, i) => (sentence.slice(i - (n - 1), i), x)
        }
      }.flatten
      new NgramModel(n, new ConditionalProbabilityDistribution[Vector[String], String](data))
    } else {
      val allTokens = tokenizedSentences.flatten
      val data = Vector.fill(allTokens.size)(Vector.empty).zip(allTokens)
      new NgramModel(n, new ConditionalProbabilityDistribution[Vector[String], String](data))
    }
  }

}
