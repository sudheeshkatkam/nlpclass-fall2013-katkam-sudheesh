package nlp.a2

import dhg.util.FileUtil._
import dhg.util.CollectionUtil._
import nlpclass.NaiveBayesTrainerToImplement
import nlpclass.NoOpFeatureExtender
import nlpclass.FeatureExtender

object InvalidPosLabel extends Exception {}

object MissingInstances extends Exception {}

object NaiveBayes {

  def main(args: Array[String]): Unit = {
    try {
      val argsList = args.toList

      val trainingInstances =
        if (argsList.indexOf("--train") != -1)
          File(argsList(argsList.indexOf("--train") + 1)).readLines.map {
            line =>
              val featurePairs :+ label = line.split(",").toVector
              label.trim -> featurePairs.map(_.split("=").map(_.trim).toTuple2)
          }.toVector
        else throw MissingInstances

      val testingInstances =
        if (argsList.indexOf("--test") != -1)
          File(argsList(argsList.indexOf("--test") + 1)).readLines.map {
            line =>
              val featurePairs :+ label = line.split(",").toVector
              label.trim -> featurePairs.map(_.split("=").map(_.trim).toTuple2)
          }.toVector
        else throw MissingInstances

      val positiveLabel =
        if (argsList.indexOf("--poslab") != -1)
          argsList(argsList.indexOf("--poslab") + 1)
        else throw InvalidPosLabel

      val lambda =
        if (argsList.indexOf("--lambda") != -1)
          argsList(argsList.indexOf("--lambda") + 1).toDouble
        else 0.0

      val log =
        if (argsList.indexOf("--log") != -1)
          argsList(argsList.indexOf("--log") + 1).toBoolean
        else false

      val extend =
        if (argsList.indexOf("--extend") != -1)
          argsList(argsList.indexOf("--extend") + 1).toBoolean
        else false

      val nbt = trainer(lambda, log, extend)
      val nbm = nbt.train(trainingInstances)
      NaiveBayesScorer.score(nbm, testingInstances, positiveLabel)

    } catch {
      case InvalidPosLabel  => println("Missing positive label.")
      case MissingInstances => println("Missing test/train instances.")
    }

  }

  def trainer(lambda: Double, log: Boolean, extend: Boolean): NaiveBayesTrainerToImplement[String, String, String] = {
    if (lambda > 0) {
      if (log) {
        if (extend)
          return new LogAddLambdaNaiveBayesTrainer[String, String, String](lambda, new PpaFeatureExtender)
        else
          return new LogAddLambdaNaiveBayesTrainer[String, String, String](lambda, new NoOpFeatureExtender)
      } else {
        if (extend)
          return new AddLambdaNaiveBayesTrainer[String, String, String](lambda, new PpaFeatureExtender)
        else
          return new AddLambdaNaiveBayesTrainer[String, String, String](lambda, new NoOpFeatureExtender)
      }
    } else {
      if (log) {
        if (extend)
          return new LogAddLambdaNaiveBayesTrainer[String, String, String](0.0, new PpaFeatureExtender)
        else
          return new LogAddLambdaNaiveBayesTrainer[String, String, String](0.0, new NoOpFeatureExtender)
      } else {
        if (extend)
          return new UnsmoothedNaiveBayesTrainer[String, String, String](new PpaFeatureExtender)
        else
          return new UnsmoothedNaiveBayesTrainer[String, String, String](new NoOpFeatureExtender)
      }
    }
  }

}