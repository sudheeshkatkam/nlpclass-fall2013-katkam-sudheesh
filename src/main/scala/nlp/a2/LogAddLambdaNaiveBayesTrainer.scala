package nlp.a2

import nlpclass.NaiveBayesTrainerToImplement
import nlpclass.NaiveBayesModelToImplement
import nlpclass.ConditionalProbabilityDistributionToImplement
import nlp.a1.ProbabilityDistribution
import nlp.a1.ConditionalProbabilityDistribution
import scala.util.Random
import com.typesafe.scalalogging.log4j.Logging
import nlpclass.FeatureExtender

class LogAddLambdaNaiveBayesTrainer[Label, Feature, Value](lambda: Double, fe: FeatureExtender[Feature, Value])
  extends NaiveBayesTrainerToImplement[Label, Feature, Value] with Logging {

  def train(instances: Vector[(Label, Vector[(Feature, Value)])]): NaiveBayesModelToImplement[Label, Feature, Value] = {
    val labels = instances.map { _._1 }.toSet
    val pLabel = new ProbabilityDistribution[Label](instances.map { _._1 }.toVector)

    val featureLabelValues = lfvToFlv(instances)
    val featureValues = lfvToFv(instances)

    val pValuePlusLambda = featureLabelValues.map {
      case (feature, labelValue) =>
        (feature, new ConditionalProbabilityDistributionWithAllValues[Label, Value](labelValue, featureValues(feature), lambda))
    }

    new LogNaiveBayesModel[Label, Feature, Value](labels, pLabel, pValuePlusLambda, fe)
  }

  def lfvToFlv(instances: Vector[(Label, Vector[(Feature, Value)])]): Map[Feature, Vector[(Label, Value)]] = {
    val flvTriples = for (
      (label, fvPairs) <- instances;
      (feature, value) <- fe.extendFeatures(fvPairs)
    ) yield { (feature, label, value) }

    flvTriples.groupBy(_._1).map {
      case (feature, groupedByFeature) =>
        feature -> groupedByFeature.map { x => (x._2, x._3) }
    }
  }

  def lfvToFv(instances: Vector[(Label, Vector[(Feature, Value)])]): Map[Feature, Set[Value]] = {
    val flvTriples = for (
      (label, fvPairs) <- instances;
      (feature, value) <- fe.extendFeatures(fvPairs)
    ) yield { (feature, label, value) }

    flvTriples.groupBy(_._1).map {
      case (feature, groupedByFeature) =>
        feature -> groupedByFeature.map { x => x._3 }.toSet
    }
  }
}