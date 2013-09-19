package nlp.a1

import nlpclass.ProbabilityDistributionToImplement
import scala.util.Random

class ProbabilityDistribution[B](data: Vector[B]) extends ProbabilityDistributionToImplement[B] {
  
  val distribution = data.groupBy(x => x).map{ 
    case (value, occurrences) => (value, occurrences.size.toDouble/data.size)}.toMap
    
  val sorted_keys = distribution.keys.toVector.sortBy(x => -distribution(x))
  
  def apply(x: B): Double = {
    distribution.getOrElse(x, 0.0)
  }
  
  // really bad code
  def sample(): B = {
    val num = Random.nextDouble
    var sum = 0.0
    sorted_keys.foreach{ x =>
      sum += distribution(x)
      if (sum > num) { return x }
    }
    ???
  }
  
}