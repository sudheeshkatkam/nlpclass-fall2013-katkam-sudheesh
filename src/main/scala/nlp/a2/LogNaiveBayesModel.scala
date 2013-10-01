package nlp.a2

import nlpclass.ProbabilityDistributionToImplement
import nlpclass.ConditionalProbabilityDistributionToImplement
import nlpclass.NaiveBayesModelToImplement
import scala.math
import nlpclass.FeatureExtender

class LogNaiveBayesModel[Label, Feature, Value](
  labels: Set[Label],
  pLabel: ProbabilityDistributionToImplement[Label],
  pValue: Map[Feature, ConditionalProbabilityDistributionToImplement[Label, Value]],
  fe: FeatureExtender[Feature, Value]) extends NaiveBayesModelToImplement[Label, Feature, Value] {

  def predict(features: Vector[(Feature, Value)]): Label = {
    val labelsMap = labels.map {
      x =>
        x -> (math.log(pLabel(x)) + fe(features).foldLeft(0.0) {
          (acc, z) =>
            acc + math.log(pValue(z._1)(z._2, x))
        })
    }.toVector.sortBy(-_._2)
    
    val maximum = labelsMap.map{_._2}.max
    
    val predictions = labelsMap.map {
      case (label, prob) =>
        label -> (math.exp(prob) - maximum)
    }
    
    val total: Double = predictions.foldLeft(0.0) { (acc, lp) => acc + lp._2 }
    
    if (total != 0)
      predictions(0)._1
    else
      labels.toVector.sortBy(pLabel(_)).toVector(0)
    //predictions.maxBy(_._2)._1
  }

}