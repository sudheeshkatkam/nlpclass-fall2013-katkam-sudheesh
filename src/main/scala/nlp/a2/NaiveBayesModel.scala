package nlp.a2

import nlpclass.ProbabilityDistributionToImplement
import nlpclass.ConditionalProbabilityDistributionToImplement
import com.typesafe.scalalogging.log4j.Logging
import nlpclass.NaiveBayesModelToImplement

class NaiveBayesModel[Label, Feature, Value](
  labels: Set[Label],
  pLabel: ProbabilityDistributionToImplement[Label],
  pValue: Map[Feature, ConditionalProbabilityDistributionToImplement[Label, Value]])
  extends NaiveBayesModelToImplement[Label, Feature, Value] with Logging {
  
  def predict(features: Vector[(Feature, Value)]): Label = {
    val labelsSortedByProb = labels.map{ label => 
      label -> pLabel(label) * features.foldLeft(1.0) { (acc, fv) => 
        acc * pValue(fv._1)(fv._2, label)}
    }.toVector.sortBy(-_._2)
    
    val total: Double = labelsSortedByProb.foldLeft(0.0){ (acc, lp) => acc + lp._2}
    logger.info( 
      if(total != 0) labelsSortedByProb.map{ case (label, prob) => 
        f"${label}%-5s${prob/total}%.4f"}.mkString("  ") 
      else 
        "All posteriors are zero!"
    )
    logger.debug(
      labels.map{ label => 
        val pl = pLabel(label) 
        val prod = features.foldLeft(1.0) { (acc, fv) => 
        acc * pValue(fv._1)(fv._2, label) } 
        f"label: ${label} pLabel: ${pl}%.4f prod: ${prod}%.4f"}.mkString("\n")
    )
    labelsSortedByProb(0)._1
  }
  
  /*
   def predict(features: Vector[(Feature, Value)]): Label = {
    labels.map{ x => 
      x -> pLabel(x) * features.foldLeft(1.0) { (acc, z) => 
        acc * pValue(z._1)(z._2, x) }
    }.maxBy(_._2)._1
  }
  * */
}