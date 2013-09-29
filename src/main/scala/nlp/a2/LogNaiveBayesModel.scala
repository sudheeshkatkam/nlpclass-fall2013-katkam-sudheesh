package nlp.a2

import nlpclass.ProbabilityDistributionToImplement
import nlpclass.ConditionalProbabilityDistributionToImplement
import nlpclass.NaiveBayesModelToImplement
import scala.math

class LogNaiveBayesModel[Label, Feature, Value](
  labels: Set[Label],
  pLabel: ProbabilityDistributionToImplement[Label],
  pValue: Map[Feature, ConditionalProbabilityDistributionToImplement[Label, Value]]) extends NaiveBayesModelToImplement[Label, Feature, Value] {
  
  def predict(features: Vector[(Feature, Value)]): Label = {
    val prob_map = labels.map{ x => 
      x -> (math.log(pLabel(x)) + features.foldLeft(0.0) { (acc, z) => 
        acc + math.log(pValue(z._1)(z._2, x)) })
    }.toMap
    val max_prob = prob_map.values.max
    val predictions = prob_map.map{ case(label, prob) => 
      label -> (math.exp(prob) - max_prob)}
    predictions.maxBy(_._2)._1
  }
  
}