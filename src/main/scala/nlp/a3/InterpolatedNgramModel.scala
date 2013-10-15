package nlp.a3

import nlpclass.NgramModelToImplement
import nlp.a1.ConditionalProbabilityDistribution
import scala.math

class InterpolatedNgramModel(val n: Int, modelsAndWeights: Vector[Tuple2[NgramModelToImplement, Double]]) extends NgramModelToImplement {

  override def sentenceProb(sentenceTokens: Vector[String]): Double = {
    //assert(modelsAndWeights.foldLeft(0.0) { (acc, modelAndWeight) => acc + modelAndWeight._2 } == 1)
    
    modelsAndWeights.map {
      case (model, weight) => model.sentenceProb(sentenceTokens) + math.log(weight)
    }.reduce(logAdd(_, _))
  }

  def logAdd(x: Double, y: Double): Double = {
    if (y.isNegInfinity) 		x
    else if (x.isNegInfinity) 	y
    else if (y <= x) 			x + math.log1p(math.exp(y - x))
    else 						y + math.log1p(math.exp(x - y))
  }

  override def generate(): Vector[String] = {
    val model = modelsAndWeights.maxBy { modelAndWeight => modelAndWeight._1.n }._1
    model.generate
  }

}