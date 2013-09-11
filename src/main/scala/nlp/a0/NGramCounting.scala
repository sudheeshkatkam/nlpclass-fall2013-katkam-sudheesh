package nlp.a0

import nlpclass.NGramCountingToImplement

class NGramCounting(n: Int) extends NGramCountingToImplement {

  def countNGrams(tokens: Vector[String]): Map[Vector[String], Int] = {
    return tokens.sliding(n).toVector.groupBy(x=> x).mapValues(_.length)
  }
}