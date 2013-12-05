package nlp.a6

import nlpclass.Tree
import nlp.a1.ConditionalProbabilityDistribution
import nlp.a1.ProbabilityDistribution
import nlpclass.TreeNode

object PCKY {

  trait BackPointer
  case class Unary(child: String) extends BackPointer
  case class Binary(i: Int, leftChild: String, rightChild: String) extends BackPointer

  def apply(tokens: Vector[String],
            nonTerminals: Set[String],
            prodProbDist: ConditionalProbabilityDistribution[String, Vector[Tree]],
            rootProbDist: ProbabilityDistribution[String]): Option[Tree] = {
    val length = tokens.length
    var table = Vector.fill(length + 1, length + 1)(scala.collection.mutable.Map[String, Double]())
    var back = Vector.fill(length + 1, length + 1)(scala.collection.mutable.Map[String, BackPointer]())

    for (j <- 1 to length) {
      fillBottomLevel(tokens(j - 1), j, table, back, prodProbDist, nonTerminals)
      fillHigherLevels(j, table, back, prodProbDist, nonTerminals)
    }

    if (table(0)(length).isEmpty)
      return None
      
    val root = table(0)(length).maxBy { case (label, prob) => prob * rootProbDist(label) }._1

    if (table(0)(length).getOrElse(root, 0.0) * rootProbDist(root) == 0.0)
      None
    else
      Some(followBackPointers(root, 0, length, back))
  }

  def fillBottomLevel(token: String,
                      j: Int,
                      table: Vector[Vector[scala.collection.mutable.Map[String, Double]]],
                      back: Vector[Vector[scala.collection.mutable.Map[String, BackPointer]]],
                      prodProbDist: ConditionalProbabilityDistribution[String, Vector[Tree]],
                      nonTerminals: Set[String]) {
    for (A <- nonTerminals) {
      val prob = prodProbDist(Vector(new TreeNode(token)), A)
      if (prob > 0.0) {
        table(j - 1)(j).put(A, prob)
        back(j - 1)(j).put(A, Unary(token))
      }
    }
  }

  def fillHigherLevels(j: Int,
                       table: Vector[Vector[scala.collection.mutable.Map[String, Double]]],
                       back: Vector[Vector[scala.collection.mutable.Map[String, BackPointer]]],
                       prodProbDist: ConditionalProbabilityDistribution[String, Vector[Tree]],
                       nonTerminals: Set[String]) {
    for (i <- (0 to j - 1).reverse) {
      binaryRules(i, j, table, back, prodProbDist, nonTerminals)
      unaryRules(i, j, table, back, prodProbDist, nonTerminals)
    }
  }

  def binaryRules(i: Int,
                  j: Int,
                  table: Vector[Vector[scala.collection.mutable.Map[String, Double]]],
                  back: Vector[Vector[scala.collection.mutable.Map[String, BackPointer]]],
                  prodProbDist: ConditionalProbabilityDistribution[String, Vector[Tree]],
                  nonTerminals: Set[String]) {
    for (k <- (i + 1) to (j - 1)) {
      for (B <- table(i)(k).keys; C <- table(k)(j).keys) {
        for (A <- nonTerminals) {
          val prob = prodProbDist(Vector(new TreeNode(B), new TreeNode(C)), A)
          if (prob > 0.0) {
            val s = prob * table(i)(k).getOrElse(B, 0.0) * table(k)(j).getOrElse(C, 0.0)
            if (!table(i)(j).contains(A) || table(i)(j).getOrElse(A, 0.0) < s) {
              table(i)(j).put(A, s)
              back(i)(j).put(A, Binary(k, B, C))
            }
          }
        }
      }
    }
  }

  def unaryRules(i: Int,
                 j: Int,
                 table: Vector[Vector[scala.collection.mutable.Map[String, Double]]],
                 back: Vector[Vector[scala.collection.mutable.Map[String, BackPointer]]],
                 prodProbDist: ConditionalProbabilityDistribution[String, Vector[Tree]],
                 nonTerminals: Set[String]) {
    var done = false
    while (!done) {
      done = true
      for (B <- table(i)(j).keys) {
        for (A <- nonTerminals) {
          val prob = prodProbDist(Vector(new TreeNode(B)), A)
          if (prob > 0.0) {
            val s = prob * table(i)(j).getOrElse(B, 0.0)
            if (!table(i)(j).contains(A) || table(i)(j).getOrElse(A, 0.0) < s) {
              table(i)(j).put(A, s)
              back(i)(j).put(A, Unary(B))
              done = false
            }
          }
        }
      }
    }
  }

  def followBackPointers(A: String, i: Int, j: Int, back: Vector[Vector[scala.collection.mutable.Map[String, BackPointer]]]): Tree = {
    back(i)(j)(A) match {
      case Binary(k, b, c) =>
        TreeNode(A, Vector(followBackPointers(b, i, k, back), followBackPointers(c, k, j, back)))
      case Unary(b) =>
        if (back(i)(j).contains(b))
          TreeNode(A, Vector(followBackPointers(b, i, j, back)))
        else
          TreeNode(b)
    }
  }

}