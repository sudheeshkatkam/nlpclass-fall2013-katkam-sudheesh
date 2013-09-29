package nlp.a2

import nlpclass.NaiveBayesTrainerToImplement
import nlpclass.NaiveBayesModelToImplement
import nlp.a1.ProbabilityDistribution
import nlp.a1.ConditionalProbabilityDistribution

class UnsmoothedNaiveBayesTrainer[Label, Feature, Value]
  extends NaiveBayesTrainerToImplement[Label, Feature, Value]{
  
  def train(instances: Vector[(Label, Vector[(Feature, Value)])]): 
    NaiveBayesModelToImplement[Label, Feature, Value] = {
    val labels = instances.map{_._1}.toSet
    val pLabel = new ProbabilityDistribution[Label](instances.map{_._1}.toVector)
    //println(lfv_to_flv(instances))
    val pValue = lfvToFlv(instances).map { case (f, lv) => 
      (f, new ConditionalProbabilityDistribution[Label, Value](lv)) }
    new NaiveBayesModel[Label, Feature, Value](labels, pLabel, pValue)
  }
  
  def lfvToFlv(instances: Vector[(Label, Vector[(Feature, Value)])]): 
    Map[Feature, Vector[(Label, Value)]] = {
	//variation of Dan Garette's implementation of getCountsFunctional() for nlp.a0 

	val flvTriples =
	  for (
		(label, fvPairs) <- instances;
		(feature, value) <- fvPairs
	  ) yield { (feature, label, value) }

	flvTriples.groupBy(_._1).map{ case (feature, groupedByFeature) =>
		feature -> groupedByFeature.map{ x => (x._2, x._3) }
	}
  }

}