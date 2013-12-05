package nlp.a6

import scala.util.Random
import nlpclass.Tree
import nlp.a1.ConditionalProbabilityDistribution
import nlp.a2.ConditionalProbabilityDistributionWithAllValues
import nlpclass.TreeNode

// ConditionalProbabilityDistribution[String, Vector[Tree]]
// N = POSNTTags.size + otherNTTags.size
class SmoothedConditionalProbabilityDistribution(PWPairs: Vector[Tuple2[String, String]],
                                                 ABPairs: Vector[Tuple2[String, Vector[Tree]]],
                                                 lambda: Double, N: Int) extends ConditionalProbabilityDistribution(ABPairs) {

  object InvalidGiven extends Exception {}

  val POSNTTags = PWPairs.map(_._1).toSet
  val vocab = PWPairs.map(_._2).toSet
  println("VOCAB: " + vocab + "\n")

  val otherNTTags = ABPairs.map(_._1).toSet
  val productions = ABPairs.map(_._2).toSet
  println("PRODUCTIONS: " + productions + "\n")

  val PWDist = new ConditionalProbabilityDistributionWithAllValues(PWPairs, vocab, lambda)
  val ABDist = new ConditionalProbabilityDistributionWithAllValues(ABPairs, productions, lambda, N)

  override def apply(x: Vector[Tree], given: String): Double = {
    if (given.contains("{"))
      1.0
    else {
      if (x.head.isPos)
        PWDist(x.head.label, given)
      else
        ABDist(x, given)
    }
  }

  override def sample(given: String): Vector[Tree] = {
    if (given.contains("{"))
      Vector.empty
    else {
      if (POSNTTags.contains(given))
        Vector(TreeNode(PWDist.sample(given)))
      else
        ABDist.sample(given)
    }
  }

}