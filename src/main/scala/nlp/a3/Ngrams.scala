package nlp.a3

import dhg.util.FileUtil._
import dhg.util.CollectionUtil._
import nlpclass.Tokenize
import nlpclass.NgramModelTrainerToImplement

object MissingInstances extends Exception {}
object InvalidN extends Exception {}

object Ngrams {
  def main(args: Array[String]): Unit = {
    try {
      val argsList = args.toList

      val trainingData =
        if (argsList.indexOf("--train") != -1)
          fileTokens(argsList(argsList.indexOf("--train") + 1))
        else throw MissingInstances

      val testingData =
        if (argsList.indexOf("--test") != -1)
          fileTokens(argsList(argsList.indexOf("--test") + 1))
        else throw MissingInstances

      val n =
        if (argsList.indexOf("--n") != -1)
          argsList(argsList.indexOf("--n") + 1).toInt
        else throw InvalidN

      val lambda =
        if (argsList.indexOf("--lambda") != -1)
          argsList(argsList.indexOf("--lambda") + 1).toDouble
        else
          0

      val interp =
        if (argsList.indexOf("--interp") != -1)
          argsList(argsList.indexOf("--interp") + 1).toBoolean
        else
          false

      val trainer = get_trainer(n, lambda, interp)
      val model = trainer.train(trainingData)
      val perp = PerplexityNgramModelEvaluator(model, testingData)
      println(perp)

    } catch {
      case MissingInstances => println("Missing test/train instances.")
      case InvalidN         => println("Invalid value of n.")
    }
  }

  def fileTokens(filename: String) = {
    File(filename).readLines
      .split("")
      .flatMap(paragraph => Tokenize(paragraph.mkString(" ")))
      .map(_.map(_.toLowerCase))
      .toVector
  }

  def get_trainer(n: Int, lambda: Double, interp: Boolean): NgramModelTrainerToImplement = {
    if (interp) 			new InterpolatedNgramModelTrainer(n, lambda)
    else if (lambda > 0) 	new AddLambdaNgramModelTrainer(n, lambda)
    else 					new UnsmoothedNgramModelTrainer(n)
  }
}