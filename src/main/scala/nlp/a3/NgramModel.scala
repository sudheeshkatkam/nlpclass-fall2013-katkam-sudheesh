package nlp.a3

import nlpclass.NgramModelToImplement
import nlpclass.ConditionalProbabilityDistributionToImplement
import nlp.a1.ConditionalProbabilityDistribution
import scala.math

class NgramModel(val n: Int, ngramProbs: ConditionalProbabilityDistributionToImplement[Vector[String], String]) extends NgramModelToImplement {

  override def sentenceProb(sentenceTokens: Vector[String]): Double = {
    val sentence =
      if (n > 1) Vector.fill(n - 1)("<S>") ++ sentenceTokens :+ "<E>"
      else sentenceTokens

    val probabilities = sentence.zipWithIndex.drop(n - 1).map {
      case (x, i) => ngramProbs(x, sentence.slice(i - (n - 1), i))
    }

    probabilities.foldLeft(0.0) { (acc, p) => acc + math.log(p) }
  }

  override def generate(): Vector[String] = {
    var sentence = Vector.fill(n - 1)("<S>")
    if (n > 1) {
      var word = ngramProbs.sample(sentence.toVector)
      while (word != "<E>") {
        sentence = sentence :+ word
        word = ngramProbs.sample(sentence.takeRight(n - 1))
      }
      sentence = sentence.drop(n - 1)
    } else {
      sentence = Vector.tabulate(10)(i => ngramProbs.sample(Vector.empty))
    }
    sentence.toVector
  }

}