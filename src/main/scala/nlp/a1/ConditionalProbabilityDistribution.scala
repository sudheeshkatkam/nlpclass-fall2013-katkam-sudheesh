package nlp.a1

import nlpclass.ConditionalProbabilityDistributionToImplement
import scala.util.Random

object InvalidGiven extends Exception { }

class ConditionalProbabilityDistribution[A, B](data: Vector[Tuple2[A, B]]) extends ConditionalProbabilityDistributionToImplement[A, B] {

  //val sorted_keys = distribution.keys.toVector.sortBy(x => -distribution(x))
  val distribution = data.groupBy(x => x._1).map{ 
    case(v, groupedByV) => v -> groupedByV.groupBy(y => y).map{
      case(k, occurrencesOfK) => k._2 -> (occurrencesOfK.size.toDouble/groupedByV.size)
      }
    }
    
  def apply(x: B, given: A): Double = {
    val temp = distribution.getOrElse(given, scala.collection.immutable.Map.empty)
    temp.getOrElse(x, 0.0)
  }
  
  // really bad code
  def sample(given: A): B = {
    try{
	    val temp = distribution.getOrElse(given, scala.collection.immutable.Map.empty)
	    if (temp ==  scala.collection.immutable.Map.empty) { throw InvalidGiven }
	    val sorted_keys = temp.keys.toVector.sortBy(x => -temp(x))
	    val num = Random.nextDouble
	    var sum = 0.0
	    sorted_keys.foreach{ x =>
	      sum += temp(x)
	      if (sum > num) { return x }
	    }
    } catch {
      case InvalidGiven => println(given + " is unknown.")
    }
    ???
  }
}