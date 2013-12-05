package nlp.a6

import nlpclass.ParserTrainer
import nlpclass.Tree

class AddLambdaPcfgParserTrainer(lambda: Double) extends ParserTrainer {

  def train(trees: Vector[Tree]): PcfgParser = {
    // roots: Vector[String], lambda: Double, N: Int, C: Int
    val roots = trees.map { t => t.label }
    val subTrees = trees.map { t =>
      val tree = Cnf.convertTree(t)
      TreeUtilities.allSubTrees(tree)
    }.flatten

    val posTrees = trees.map { t =>
      val tree = Cnf.convertTree(t)
      TreeUtilities.allPosTrees(tree)
    }.flatten

    val nonTerminals = subTrees.map { tree => tree.label }.toSet
    val compoundNonTerminals = subTrees.filter { tree => tree.label.contains("{") }.toSet
    println("N: " + nonTerminals.size + " NON TERMINALS: " + nonTerminals + "\n")
    println("C: " + compoundNonTerminals.size + " COMPOUND NON TERMINALS: " + compoundNonTerminals + "\n")

    val rootProbDist = new SmoothedProbabilityDistribution(roots, lambda, nonTerminals.size, compoundNonTerminals.size)

    val PWPairs = posTrees.map { tree => (tree.label, tree.children.head.label) }
    println("PW PAIRS: " + PWPairs + "\n")
    val ABPairs = subTrees.filterNot { tree => tree.label.contains("{") || tree.isPos }.map { tree => (tree.label, tree.children) }
    println("AB PAIRS: " + ABPairs + "\n")
    val prodProbDist = new SmoothedConditionalProbabilityDistribution(PWPairs, ABPairs, lambda, nonTerminals.size)

    new PcfgParser(nonTerminals, prodProbDist, rootProbDist)
  }

}