package nlp.a6

import nlpclass.ParserTrainer
import nlpclass.Tree
import nlp.a1.ConditionalProbabilityDistribution
import nlp.a1.ProbabilityDistribution
import nlpclass.TreeNode

class UnsmoothedPcfgParserTrainer extends ParserTrainer {

  def train(trees: Vector[Tree]): PcfgParser = {

    val rootData = trees.map { t => t.label }
    val rootProbDist = new ProbabilityDistribution(rootData)

    val subTrees = trees.map { t =>
      val tree = Cnf.convertTree(t)
      TreeUtilities.allSubTrees(tree)
    }.flatten

    val prodData = subTrees.map { tree => (tree.label, tree.children) }
    val nonTerminals = prodData.map { case (label, children) => label }.toSet
    val prodProbDist = new ConditionalProbabilityDistribution(prodData)

    new PcfgParser(nonTerminals, prodProbDist, rootProbDist)
  }

}