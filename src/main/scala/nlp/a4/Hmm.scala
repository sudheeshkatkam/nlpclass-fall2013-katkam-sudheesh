package nlp.a4

import dhg.util.FileUtil._
import dhg.util.CollectionUtil._

object MissingInstances extends Exception {}

object Hmm {

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

      val lambda =
        if (argsList.indexOf("--lambda") != -1)
          argsList(argsList.indexOf("--lambda") + 1).toDouble
        else
          0

      val trainer = getTrainer(lambda)
      val model = trainer.train(trainingData)
      
      ModelEvaluator[String, String](model, testingData)
    } catch {
      case MissingInstances => println("Missing test/train instances.")
    }
  }

  private def fileTokens(filename: String) = {
    File(filename).readLines.map {
      line =>
        val pairs = line.split(" ").toVector
        pairs.map { pair =>
          val tuple = pair.splitAt(pair.indexOf("|"))
          (tuple._1, tuple._2.drop(1))
        }
    }.toVector
  }

  private def getTrainer(lambda: Double) = {
    if (lambda > 0)
      new AddLambdaSmoothedHmmTrainer[String, String]("<S>", "<S>", "<E>", "<E>", lambda)
    else
      new UnsmoothedHmmTrainer[String, String]("<S>", "<S>", "<E>", "<E>")
  }
  //val data = File("./data/ptbtag_train.txt").readLines.map { line => val pairs = line.split(" ").toVector; pairs.map { pair => val tuple = pair.splitAt(pair.indexOf("|")); (tuple._1, tuple._2.drop(1)) } }.toVector
  
}