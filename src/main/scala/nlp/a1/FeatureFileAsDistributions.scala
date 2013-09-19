package nlp.a1

import nlpclass.FeatureFileAsDistributionsToImplement
import nlpclass.ConditionalProbabilityDistributionToImplement
import nlpclass.ProbabilityDistributionToImplement

object FeatureFileAsDistributions extends FeatureFileAsDistributionsToImplement {

  def fromFile(filename: String): 
	  		(Set[String], ProbabilityDistributionToImplement[String], 
	  		    Map[String, ConditionalProbabilityDistributionToImplement[String, String]]) = {
    ???
  }


}