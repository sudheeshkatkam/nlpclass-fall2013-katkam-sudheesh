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
    val prodProbDist = new ConditionalProbabilityDistribution(prodData)

    return new PcfgParser(prodProbDist, rootProbDist)
  }

}