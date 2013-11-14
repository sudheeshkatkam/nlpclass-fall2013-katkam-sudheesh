package nlp.a5

import nlpclass.FeatureExtender
import nlpclass.CompositeFeatureExtender

class HcrFeatureExtender(posWords: Set[String] = Set[String](),
                         negWords: Set[String] = Set[String](),
                         stopWords: Set[String] = Set[String]()) extends FeatureExtender[String, String] {

  def extendFeatures(features: Vector[(String, String)]): Vector[(String, String)] = {
    val newFeatures = (new CompositeFeatureExtender(
      Vector(
        new LowerCaseFeatureExtender,
        new WordBasedFeatureExtender,
        new StopWordFeatureExtender(stopWords),
        new HashTagBasedFeatureExtender,
        new PolarityLexiconFeatureExtender(posWords, negWords),
        new PartyBasedFeatureExtender,
        new TargetBasedFeatureExtender,
        new QuestionFeatureExtender,
        new AllCapsFeatureExtender,
        new NameFeatureExtender,
        //new AllHashTagValueCombinationsFeatureExtender,
        new AllTargetValueCombinationsFeatureExtender,
        new AllUsernameValueCombinationsFeatureExtender))).extendFeatures(features)
    //println(newFeatures)
    //System.exit(0);
    newFeatures
  }

}

class WordBasedFeatureExtender extends FeatureExtender[String, String] {

  val WordRegex = """[a-zA-Z]+""".r
  def extendFeatures(features: Vector[(String, String)]): Vector[(String, String)] = {
    features.map {
      case (feature, value) =>
        value match {
          case WordRegex() => Vector(("word", value), (feature, value))
          case _           => Vector(("non-word", value), (feature, value))
        }
    }.flatten
  }

}

class LowerCaseFeatureExtender extends FeatureExtender[String, String] {

  def extendFeatures(features: Vector[(String, String)]): Vector[(String, String)] = {
    val newFeatures = features.map { case (feature, value) => (feature, value.toLowerCase) }

    features ++ newFeatures
  }

}

class NameFeatureExtender extends FeatureExtender[String, String] {

  def extendFeatures(features: Vector[(String, String)]): Vector[(String, String)] = {
    val newFeatures =
      Marker(features.filter { case (feature, value) => value.startsWith("@") }, "RT")

    features ++ newFeatures
  }

}

class AllCapsFeatureExtender extends FeatureExtender[String, String] {

  def extendFeatures(features: Vector[(String, String)]): Vector[(String, String)] = {
    val newFeatures = Marker(features.filter { case (feature, value) => value.foldLeft(true) { (acc, char) => (acc && char.isUpper) } }, "caps")

    features ++ newFeatures
  }

}

class PolarityLexiconFeatureExtender(posWords: Set[String], negWords: Set[String]) extends FeatureExtender[String, String] {

  def extendFeatures(features: Vector[(String, String)]): Vector[(String, String)] = {
    val newFeatures =
      Marker(features.filter { case (feature, value) => posWords.contains(value) }, "positive") ++
        Marker(features.filter { case (feature, value) => negWords.contains(value) }, "negative")

    features ++ newFeatures
  }

}

class StopWordFeatureExtender(stopWords: Set[String]) extends FeatureExtender[String, String] {

  def extendFeatures(features: Vector[(String, String)]): Vector[(String, String)] = {
    val newFeatures =
      Marker(features.filter { case (feature, value) => stopWords.contains(value) }, "stop") ++
        Marker(features.filter { case (feature, value) => !stopWords.contains(value) }, "non-stop")

    features ++ newFeatures
  }

}

class QuestionFeatureExtender extends FeatureExtender[String, String] {

  def extendFeatures(features: Vector[(String, String)]): Vector[(String, String)] = {
    val newFeatures = Marker(features.filter { case (feature, value) => value.endsWith("?") }, "question")

    features ++ newFeatures
  }

}

class PartyBasedFeatureExtender extends FeatureExtender[String, String] {

  def extendFeatures(features: Vector[(String, String)]): Vector[(String, String)] = {
    val newFeatures =
      Marker(features.filter { case (feature, value) => value.contains("gop") || value.contains("tea") || value.contains("kill") || value.contains("repub") || value.contains("tcot") || value.contains("p2") }, "republican") ++
        Marker(features.filter { case (feature, value) => value.contains("dem") || value.contains("save") }, "democractic")

    features ++ newFeatures
  }

}

class TargetBasedFeatureExtender extends FeatureExtender[String, String] {

  def extendFeatures(features: Vector[(String, String)]): Vector[(String, String)] = {
    val target +: tokens = features
    val newFeatures =
      features.filter { case (feature, value) => value.startsWith("#") }.map { case (feature, value) => (target._1 + "hashtag", target._2 + value) }

    features ++ newFeatures
  }

}

class HashTagBasedFeatureExtender extends FeatureExtender[String, String] {

  def extendFeatures(features: Vector[(String, String)]): Vector[(String, String)] = {
    val newFeatures =
      Marker(features.filter { case (feature, value) => value.startsWith("#") }, "hashtag") ++
        Marker(features.filter { case (feature, value) => !value.startsWith("#") && feature == "word" }, "non-hashtag")

    features ++ newFeatures
  }

}

class AllHashTagValueCombinationsFeatureExtender extends FeatureExtender[String, String] {

  def extendFeatures(features: Vector[(String, String)]): Vector[(String, String)] = {
    val hashTagPairs =
      features.filter { case (feature, value) => feature == "hashtag" }
    val nonHashTagPairs =
      features.filter { case (feature, value) => feature == "non-hashtag" }
    val newFeatures =
      for (y <- hashTagPairs; x <- nonHashTagPairs) yield ("HW", y._2 + x._2)

    features ++ newFeatures
  }

}

class AllTargetValueCombinationsFeatureExtender extends FeatureExtender[String, String] {

  def extendFeatures(features: Vector[(String, String)]): Vector[(String, String)] = {
    val target +: tokens = features
    //val newFeatures = for (y <- tokens) yield (target._1 + y._1, target._2 + y._2)
    val newFeatures =
      tokens.filter { case (feature, value) => feature == "word" || feature == "non-word" }.map { case (feature, value) => (target._1 + feature, target._2 + value) }
    features ++ newFeatures
  }

}

class AllUsernameValueCombinationsFeatureExtender extends FeatureExtender[String, String] {

  def extendFeatures(features: Vector[(String, String)]): Vector[(String, String)] = {
    val target +: userName +: tokens = features
    //val newFeatures = for (y <- tokens) yield (userName._1 + y._1, userName._2 + y._2)

    val newFeatures =
      tokens.filter { case (feature, value) => feature == "word" || feature == "non-word" }.map { case (feature, value) => (userName._1 + feature, userName._2 + value) }
    features ++ newFeatures
  }

}

object Marker {

  def apply(features: Vector[(String, String)], tag: String) = {
    features.map { case (feature, value) => (tag, value) }
  }

}