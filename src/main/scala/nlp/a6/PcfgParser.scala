package nlp.a6

import nlpclass.Parser
import nlpclass.Tree
import nlp.a1.ConditionalProbabilityDistribution
import nlp.a1.ProbabilityDistribution

class PcfgParser(prodProbDist: ConditionalProbabilityDistribution[String, Vector[Tree]],
                 rootProbDist: ProbabilityDistribution[String]) extends Parser {

  def likelihood(t: Tree): Double = {
    val tree = Cnf.convertTree(t)
    val subTrees = TreeUtilities.allSubTrees(tree)
    val prodProb = subTrees.map { st => math.log(prodProbDist(st.children, st.label)) }.reduce(_ + _)
    return math.log(rootProbDist(t.label)) + prodProb
  }

  def parse(tokens: Vector[String]): Option[Tree] = ???

  def generate(): Tree = ???

}