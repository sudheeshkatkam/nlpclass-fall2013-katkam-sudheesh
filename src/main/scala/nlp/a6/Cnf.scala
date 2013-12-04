package nlp.a6

import nlpclass.Tree
import nlpclass.TreeNode

object Cnf {
  def convertTree(t: Tree): Tree = {
    if (t.isPos)
      return t

    if (t.children.length > 1) {
      var tree = t

      while (tree.children.length > 2) {
        val others :+ first :+ second = tree.children

        val label = "{" + first.label + "+" + second.label + "}"
        val children = Vector(first, second)
        val newNode = TreeNode(label, children)

        tree = TreeNode(t.label, others :+ newNode)
      }

      val leftTree = convertTree(tree.children.head)
      val rightTree = convertTree(tree.children.last)
      TreeNode(tree.label, Vector(leftTree, rightTree))
    } else
      TreeNode(t.label, Vector(convertTree(t.children.head)))
  }

  def undo(t: Tree): Tree = {
    if (t.isPos)
      return t

    if (t.children.head.isPos && t.children.last.isPos)
      return t

    val leftChild = t.children.head
    val leftTree = undo(leftChild)
    val rightChild = t.children.last
    val rightTree = undo(rightChild)

    if (rightChild.label.contains("{"))
      return TreeNode(t.label, leftTree +: rightTree.children)
    else
      return TreeNode(t.label, Vector(leftTree, rightTree))
  }

}