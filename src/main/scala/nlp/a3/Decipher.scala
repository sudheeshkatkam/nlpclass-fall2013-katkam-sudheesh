package nlp.a3

import nlpclass.DecipherToImplement
import nlpclass.NgramModelToImplement
import nlp.a2.ConditionalProbabilityDistributionWithAllValues

object Decipher extends nlpclass.DecipherToImplement {

  def Vectorify(word: String): Vector[String] = word.toCharArray.toVector.map(_.toString)

  override def train(text: Vector[Vector[String]], n: Int, lambda: Double): NgramModelToImplement = {
    val words = text.flatten
    val tokenizedWords = words.map { word => Vectorify(word) }

    //(new InterpolatedNgramModelTrainer(n, lambda)).train(tokenizedWords)
    (new AddLambdaNgramModelTrainer(n, lambda)).train(tokenizedWords)
  }

  override def swapLetters(i: Int, j: Int, cipherKey: Vector[String]): Vector[String] = {
    if (j < i)
      swap(i, j, cipherKey)
    else
      swap(j, i, cipherKey)
  }

  def swap(i: Int, j: Int, cipherKey: Vector[String]): Vector[String] =
    ((cipherKey.slice(0, j)
      :+ cipherKey(i))
      ++ cipherKey.slice(j + 1, i)
      :+ cipherKey(j)) ++ cipherKey.slice(i + 1, cipherKey.size)

  override def scoreCipherKey(cipherText: Vector[Vector[String]],
                     cipherKey: Vector[String],
                     ngramModel: NgramModelToImplement): Double = {

    val decipheredText = decipher(cipherKey, cipherText)
    val text = decipheredText.map {
      sentence =>
        sentence.map { word => Vectorify(word) }
    }.flatten

    PerplexityNgramModelEvaluator(ngramModel, text)
  }

}