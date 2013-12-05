package nlp.a6

import nlpclass.Parser
import nlpclass.Tree
import nlp.a1.ConditionalProbabilityDistribution
import nlp.a1.ProbabilityDistribution

class PcfgParser(nonTerminals: Set[String],
                 prodProbDist: ConditionalProbabilityDistribution[String, Vector[Tree]],
                 rootProbDist: ProbabilityDistribution[String]) extends Parser {

  def likelihood(t: Tree): Double = {
    val tree = Cnf.convertTree(t)
    val subTrees = TreeUtilities.allSubTrees(tree)
    val prodProb = subTrees.map { st => math.log(prodProbDist(st.children, st.label)) }.reduce(_ + _)

    math.log(rootProbDist(t.label)) + prodProb
  }

  def parse(tokens: Vector[String]): Option[Tree] = {
    val tree = PCKY(tokens, nonTerminals, prodProbDist, rootProbDist)
    tree match {
      case Some(t) => Some(Cnf.undo(t))
      case None    => None
    }
  }

  def generate(): Tree = ???

}