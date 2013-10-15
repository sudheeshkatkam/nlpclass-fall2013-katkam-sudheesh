package nlp.a3

import nlpclass.NgramModelTrainerToImplement
import nlpclass.NgramModelToImplement
import scala.math

class InterpolatedNgramModelTrainer(n: Int, lambda: Double) extends NgramModelTrainerToImplement {

  override def train(tokenizedSentences: Vector[Vector[String]]): NgramModelToImplement = {
    val trainers = Vector.tabulate(n) { i => new AddLambdaNgramModelTrainer(i + 1, lambda) }
    val models = trainers.map { trainer => trainer.train(tokenizedSentences) }

    val denom = math.pow(2, n) - 1
    val modelsAndWeights = Vector.tabulate(n) { i => (models(i), (math.pow(2, i) / denom)) }

    new InterpolatedNgramModel(n, modelsAndWeights)
  }

}