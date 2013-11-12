package nlp.a5

import dhg.util.FileUtil._
import dhg.util.CollectionUtil._
import nlp.a2.ClassifierScorer

object MissingInstances extends Exception {}

object SentimentLexiconRatio {

  def main(args: Array[String]): Unit = {
    try {
      val argsList = args.toList

      val posWords =
        if (argsList.indexOf("--pos") != -1)
          fileTokens(argsList(argsList.indexOf("--pos") + 1))
        else throw MissingInstances

      val negWords =
        if (argsList.indexOf("--neg") != -1)
          fileTokens(argsList(argsList.indexOf("--neg") + 1))
        else throw MissingInstances

      val testData =
        if (argsList.indexOf("--test") != -1)
          testFileTokens(argsList(argsList.indexOf("--test") + 1))
        else throw MissingInstances

      ClassifierScorer.score(new LexiconRatioSentimentClassifier(posWords, negWords), testData)
    } catch {
      case MissingInstances => println("Missing pos/neg/test instances.")
    }
  }

  private def fileTokens(filename: String) = {
    File(filename).readLines.filterNot(line => line.startsWith(";") || line.isEmpty).toSet
  }

  private def wordSet(filename: String) = {
    File(filename).readLines.map(_.trim).dropWhile(_.startsWith(";")).filter(_.nonEmpty).toSet
  }

  private def testFileTokens(filename: String) = {
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