package nlp.a6

import nlpclass.Tree
import nlpclass.TreeNode

object TreeUtilities {

  def allSubTrees(t: Tree): Vector[Tree] = {
    if (t.isPos)
      return Vector(t)

    if (t.children.length == 2) {
      val leftSubTrees = allSubTrees(t.children.head)
      val rightSubTrees = allSubTrees(t.children.last)
      val thisTree = new TreeNode(t.label, Vector(new TreeNode(t.children.head.label), new TreeNode(t.children.last.label)))
      return leftSubTrees ++ rightSubTrees :+ thisTree
    } else {
      val subTrees = allSubTrees(t.children.last)
      val thisTree = new TreeNode(t.label, Vector(new TreeNode(t.children.head.label)))
      return subTrees :+ thisTree
    }

  }

}