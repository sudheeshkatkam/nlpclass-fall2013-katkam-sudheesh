package nlp.a1

import nlpclass.FeatureFileAsDistributionsToImplement
import nlpclass.ConditionalProbabilityDistributionToImplement
import nlpclass.ProbabilityDistributionToImplement
import nlp.a0.CountFeatures
import dhg.util.FileUtil._
import dhg.util.CollectionUtil._

object FeatureFileAsDistributions extends FeatureFileAsDistributionsToImplement {

  def fromFile(filename: String): 
	  		(Set[String], ProbabilityDistributionToImplement[String], 
	  		    Map[String, ConditionalProbabilityDistributionToImplement[String, String]]) = {
    
    val instances =
	    File(filename).readLines.map { line =>
					val featurePairs :+ label = line.split(",").toVector // split into feature pairs and a final label
					label.trim -> featurePairs.map(_.split("=").map(_.trim).toTuple2) // turn feature pairs into tuples
		}.toVector
	val labels = instances.map { case (label, _) => label }
	val valueCountsByLabelByFeature = CountFeatures.getCountsFunctional(instances)
	val pd = new ProbabilityDistribution[String](labels.toVector)
	val cpd_map = valueCountsByLabelByFeature.map { case (k, v) => 
	  k -> new ConditionalProbabilityDistribution[String, String](valueCountsByLabelByFeature(k).map{ case (l, vc_map) =>
			    vc_map.map { case (v, count) =>
			      (1 to count).map{_ => (l, v)}.toVector
			    }.flatten
			  }.flatten.toVector)
	}
	(labels.toSet, pd, cpd_map)
  }


}