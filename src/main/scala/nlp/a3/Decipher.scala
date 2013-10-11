package nlp.a3

import nlpclass.DecipherToImplement
import nlpclass.NgramModelToImplement

class Decipher extends nlpclass.DecipherToImplement{
  
  def train(text: Vector[Vector[String]], n: Int, lambda: Double): NgramModelToImplement = {
    ???
  }

  def swapLetters(i: Int, j: Int, cipherKey: Vector[String]): Vector[String] = {
    ???
  }

  def scoreCipherKey(cipherText: Vector[Vector[String]], cipherKey: Vector[String], ngramModel: NgramModelToImplement): Double  = {
    ???
  }
}