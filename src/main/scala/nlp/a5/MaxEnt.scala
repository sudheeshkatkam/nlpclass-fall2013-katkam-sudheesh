package nlp.a5

import nlpclass.MaxEntModel
import nlpclass.MaxEntModelTrainer
import dhg.util.FileUtil._
import dhg.util.CollectionUtil._
import nlp.a2.ClassifierScorer
import nlp.a2.NaiveBayes

object MaxEnt {

  def main(args: Array[String]): Unit = {
    try {
      val argsList = args.toList

      val trainData =
        if (argsList.indexOf("--test") != -1)
          fileTokens(argsList(argsList.indexOf("--train") + 1))
        else throw MissingInstances

      val testData =
        if (argsList.indexOf("--test") != -1)
          fileTokens(argsList(argsList.indexOf("--test") + 1))
        else throw MissingInstances

      val posWords =
        if (argsList.indexOf("--pos") != -1)
          wordSet(argsList(argsList.indexOf("--pos") + 1))
        else Set[String]()

      val negWords =
        if (argsList.indexOf("--neg") != -1)
          wordSet(argsList(argsList.indexOf("--neg") + 1))
        else Set[String]()

      val stopWords =
        if (argsList.indexOf("--stop") != -1)
          wordSet(argsList(argsList.indexOf("--stop") + 1))
        else Set[String]()

      val sigma =
        if (argsList.indexOf("--sigma") != -1)
          argsList(argsList.indexOf("--sigma") + 1).toDouble
        else
          0

      val key =
        if (argsList.indexOf("--extend") != -1)
          argsList(argsList.indexOf("--extend") + 1)
        else "none"

      val trainer = new MaxEntModelTrainer(sigma = sigma, featureExtender = NaiveBayes.getFeatureExtender(key, posWords, negWords, stopWords))
      val model = trainer.train(trainData)
      ClassifierScorer.score(model, testData)
    } catch {
      case MissingInstances => println("Missing pos/neg/test instances.")
    }
  }

  private def wordSet(filename: String) = {
    File(filename).readLines.filterNot(line => line.startsWith(";") || line.isEmpty).toSet
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

}