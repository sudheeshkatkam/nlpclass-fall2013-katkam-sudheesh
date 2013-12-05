package nlp.a6

import nlp.a1.ProbabilityDistribution

class SmoothedProbabilityDistribution(roots: Vector[String], lambda: Double, N: Int, C: Int) extends ProbabilityDistribution(roots) {

  val rootFreq = roots.groupBy { x => x }.map { case (root, groupedByRoot) => (root, groupedByRoot.size) }
  println("ROOT FREQUENCIES: " + rootFreq + "\n")

  override def apply(x: String): Double = {
    if (rootFreq.contains(x))
      ((rootFreq(x) + lambda) / (roots.length + (lambda * (N - C))))
    else
      0.0
  }

}