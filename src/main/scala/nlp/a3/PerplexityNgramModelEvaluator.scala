package nlp.a3

import nlpclass.NgramModelEvaluator
import nlpclass.NgramModelToImplement
import scala.math

object PerplexityNgramModelEvaluator extends NgramModelEvaluator {

  override def apply(model: NgramModelToImplement, tokenizedSentences: Vector[Vector[String]]): Double = {
    val numer = -tokenizedSentences.foldLeft(0.0) { (acc, x) => acc + model.sentenceProb(x) }
    val denom =  tokenizedSentences.foldLeft(0.0) { (acc, x) => acc + x.size }
    math.exp(numer / denom)
  }

}