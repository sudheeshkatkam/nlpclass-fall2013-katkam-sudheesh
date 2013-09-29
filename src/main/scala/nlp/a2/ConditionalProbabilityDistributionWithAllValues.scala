package nlp.a2

import nlpclass.ConditionalProbabilityDistributionToImplement
import com.typesafe.scalalogging.log4j.Logging
import scala.util.Random

class ConditionalProbabilityDistributionWithAllValues[A, B]
  (abPairs: Vector[Tuple2[A, B]], allValues: Set[B], lambda: Double) 
  extends ConditionalProbabilityDistributionToImplement[A, B] with Logging {
	  
	  object InvalidGiven extends Exception { }

      val frequencyDistribution = abPairs.groupBy(x => x._1).map { case (a, groupedByA) =>
	    a -> groupedByA.groupBy(y => y).map{ case (ab, occurrencesOfAB) =>
	      ab._2 -> occurrencesOfAB.size}
	  }
	  
      val frequencySumDistribution = frequencyDistribution.map{ case (a, mapOfA) =>
        a -> allValues.foldLeft(0){ (acc, b) => 
          acc + frequencyDistribution(a).getOrElse(b, 0)}
      }
      
	  def apply(x: B, given: A): Double = {
	    val fDist = frequencyDistribution.getOrElse(given, scala.collection.immutable.Map.empty)
	    val fSum = frequencySumDistribution.getOrElse(given, 0)
	    val freq = fDist.getOrElse(x, 0)
	    logger.debug("P(" + x + " | " + given + "):" + 
	        " num: " + (freq + lambda) + 
	        " den: " + fSum + " + " + (allValues.size * lambda))
	    (freq + lambda).toDouble/(fSum + (allValues.size * lambda))
	  }
	  
	  def sample(given: A): B = {
	    try{
	        val distribution = abPairs.map{ case(a, b) =>
	          a -> frequencyDistribution(a).map{ case(b, i) =>
	            b -> i.toDouble/frequencySumDistribution(a) }
	        }.toMap
		    val dist = distribution.getOrElse(given, scala.collection.immutable.Map.empty)
		    if (dist ==  scala.collection.immutable.Map.empty) { throw InvalidGiven }
		    val sorted_keys = dist.keys.toVector.sortBy(x => -dist(x))
		    val num = Random.nextDouble		  
		    sorted_keys.foldLeft(0.0){ (acc, v) => 
		      if(acc + dist(v) > num) 
		        return v
		      acc + dist(v) }
		    
		    /* var sum = 0.0
		    sorted_keys.foreach{ x =>
		      sum += dist(x)
		      if (sum > num) { return x }
		    }
		    */
	    } catch {
	      case InvalidGiven => println(given + " is unknown.")
	    }
	    ???
	  }
}