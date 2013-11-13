package nlp.a2

import dhg.util.FileUtil._
import dhg.util.CollectionUtil._
import nlpclass.NaiveBayesTrainerToImplement
import nlpclass.NoOpFeatureExtender
import nlpclass.FeatureExtender
import nlp.a5.HcrFeatureExtender

object MissingInstances extends Exception {}

object NaiveBayes {

  def main(args: Array[String]): Unit = {
    try {
      val argsList = args.toList

      val trainingInstances =
        if (argsList.indexOf("--train") != -1)
          fileTokens(argsList(argsList.indexOf("--train") + 1))
        else throw MissingInstances

      val testingInstances =
        if (argsList.indexOf("--test") != -1)
          fileTokens(argsList(argsList.indexOf("--test") + 1))
        else throw MissingInstances

      val positiveLabel =
        if (argsList.indexOf("--poslab") != -1)
          argsList(argsList.indexOf("--poslab") + 1)
        else "none"

      val lambda =
        if (argsList.indexOf("--lambda") != -1)
          argsList(argsList.indexOf("--lambda") + 1).toDouble
        else 0.0

      val posWords =
        if (argsList.indexOf("--pos") != -1)
          wordSet(argsList(argsList.indexOf("--pos") + 1))
        else Set[String]()

      val negWords =
        if (argsList.indexOf("--neg") != -1)
          wordSet(argsList(argsList.indexOf("--neg") + 1))
        else Set[String]()

      val log =
        if (argsList.indexOf("--log") != -1)
          argsList(argsList.indexOf("--log") + 1).toBoolean
        else false

      val key =
        if (argsList.indexOf("--extend") != -1)
          argsList(argsList.indexOf("--extend") + 1)
        else "none"

      val trainer = getTrainer(lambda, log, key, posWords, negWords)
      val model = trainer.train(trainingInstances)

      // NaiveBayesScorer.score(model, testingInstances, positiveLabel)
      ClassifierScorer.score(model, testingInstances)
    } catch {
      case MissingInstances => println("Missing test/train instances.")
    }

  }

  private def fileTokens(filename: String) = {
    File(filename).readLines.map {
      line =>
        val featurePairs :+ label = line.split(",").toVector
        label.trim -> featurePairs.map {
          pair =>
            val rawPair = pair.splitAt(pair.indexOf("="))
            (rawPair._1.trim.toLowerCase, rawPair._2.drop(1).toLowerCase)
        }
    }.toVector
  }

  private def getTrainer(lambda: Double, log: Boolean, key: String, posWords: Set[String], negWords: Set[String]): NaiveBayesTrainerToImplement[String, String, String] = {
    val fe = getFeatureExtender(key, posWords, negWords)
    if (log) {
      return new LogAddLambdaNaiveBayesTrainer[String, String, String](lambda, fe)
    } else {
      if (lambda > 0)
        return new AddLambdaNaiveBayesTrainer[String, String, String](lambda, fe)
      else
        return new UnsmoothedNaiveBayesTrainer[String, String, String](fe)
    }
  }

  private def wordSet(filename: String) = {
    File(filename).readLines.filterNot(line => line.startsWith(";") || line.isEmpty).toSet
  }

  def getFeatureExtender(key: String,
                         posWords: Set[String],
                         negWords: Set[String],
                         stopWords: Set[String] = Set[String]()): FeatureExtender[String, String] = {
    key match {
      case "ppa" => new PpaFeatureExtender
      case "hcr" => new HcrFeatureExtender(posWords, negWords, stopWords)
      case _     => new NoOpFeatureExtender
    }
  }

}