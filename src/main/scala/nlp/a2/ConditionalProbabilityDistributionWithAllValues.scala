package nlp.a2

import nlpclass.ConditionalProbabilityDistributionToImplement
import com.typesafe.scalalogging.log4j.Logging
import scala.util.Random

class ConditionalProbabilityDistributionWithAllValues[A, B](pairs: Vector[Tuple2[A, B]], allSeconds: Set[B], lambda: Double)
  extends ConditionalProbabilityDistributionToImplement[A, B] with Logging {

  object InvalidGiven extends Exception {}

  val frequencyDistribution = pairs.groupBy(x => x._1).map {
    case (first, groupedByFirst) =>
      first -> groupedByFirst.groupBy(y => y).map {
        case (pairs, occurrencesOfPairs) =>
          pairs._2 -> occurrencesOfPairs.size
      }
  }

  val frequencySumDistribution = frequencyDistribution.map {
    case (first, mapFirst) =>
      first -> allSeconds.foldLeft(0) {
        (acc, second) =>
          acc + frequencyDistribution(first).getOrElse(second, 0)
      }
  }

  val distribution = pairs.map(_._1).to[Set].map { first =>
    first -> frequencyDistribution(first).map {
      case (second, frequency) =>
        second -> frequency.toDouble / frequencySumDistribution(first)
    }
  }.toMap

  def apply(x: B, given: A): Double = {
    val fDist = frequencyDistribution.getOrElse(given, scala.collection.immutable.Map.empty)
    val fSum = frequencySumDistribution.getOrElse(given, 0)
    val freq = fDist.getOrElse(x, 0)
    logger.debug("P(" + x + " | " + given + "):" +
      " numer: " + (freq + lambda) +
      " denom: " + fSum + " + " + (allSeconds.size * lambda))
    (freq + lambda).toDouble / (fSum + (allSeconds.size * lambda))
  }

  def sample(given: A): B = {
    try {
      val dist = distribution.getOrElse(given, scala.collection.immutable.Map.empty)
      if (dist == scala.collection.immutable.Map.empty) { throw InvalidGiven }
      val sorted_keys = dist.keys.toVector.sortBy(-dist(_))
      val num = Random.nextDouble
      var sum = 0.0
      sorted_keys.foreach { x =>
        sum += dist(x)
        if (sum > num) { return x }
      }
    } catch {
      case InvalidGiven => println(given + " is unknown.")
    }
    ???
  }
}