package nlp.a2

import nlpclass.NaiveBayesTrainerToImplement
import nlpclass.NaiveBayesModelToImplement
import nlp.a1.ProbabilityDistribution
import nlp.a1.ConditionalProbabilityDistribution
import nlpclass.FeatureExtender

class UnsmoothedNaiveBayesTrainer[Label, Feature, Value](fe: FeatureExtender[Feature, Value])
  extends NaiveBayesTrainerToImplement[Label, Feature, Value] {

  def train(instances: Vector[(Label, Vector[(Feature, Value)])]): NaiveBayesModelToImplement[Label, Feature, Value] = {
    val labels = instances.map { _._1 }.toSet
    val pLabel = new ProbabilityDistribution[Label](instances.map { _._1 }.toVector)
    val pValue = lfvToFlv(instances).map {
      case (feature, labelValue) =>
        (feature, new ConditionalProbabilityDistribution[Label, Value](labelValue))
    }

    new NaiveBayesModel[Label, Feature, Value](labels, pLabel, pValue, fe)
  }

  def lfvToFlv(instances: Vector[(Label, Vector[(Feature, Value)])]): Map[Feature, Vector[(Label, Value)]] = {
    val flvTriples =
      for (
        (label, fvPairs) <- instances;
        (feature, value) <- fe.extendFeatures(fvPairs)
      ) yield { (feature, label, value) }

    flvTriples.groupBy(_._1).map {
      case (feature, groupedByFeature) =>
        feature -> groupedByFeature.map { x => (x._2, x._3) }
    }
  }

}