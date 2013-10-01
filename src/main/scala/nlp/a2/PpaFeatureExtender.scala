package nlp.a2

import nlpclass.FeatureExtender
import nlpclass.CompositeFeatureExtender
import nlpclass.Lemmatize

class PpaFeatureExtender extends FeatureExtender[String, String] {

  def extendFeatures(features: Vector[(String, String)]): Vector[(String, String)] = {
    (new CompositeFeatureExtender(Vector(
      new AllTwoValueCombinationsFeatureExtender,
      new LemmaFeatureExtender,
      new NumberFeatureExtender,
      new SpecialCharactersFeatureExtender,
      new VNCombinationFeatureExtender,
      new CapitalizationFeatureExtender))).extendFeatures(features)
  }

}

class NumberFeatureExtender extends FeatureExtender[String, String] {

  val NumRegex = """[0-9]+\.?[0-9]*""".r
  def extendFeatures(features: Vector[(String, String)]): Vector[(String, String)] = {
    features.par.map {
      case (feature, value) =>
        value match {
          case NumRegex() => Vector(("numeric", value), (feature, value))
          case _          => Vector((feature, value))
        }
    }.seq.flatten
  }

}

class SpecialCharactersFeatureExtender extends FeatureExtender[String, String] {

  val SpecialCharRegex = """\$|\@|\%|\&|\*|\^|\~|\(|\)|\-|\_|\=|\+|\#""".r
  def extendFeatures(features: Vector[(String, String)]): Vector[(String, String)] = {
    features.par.map {
      case (feature, value) =>
        value match {
          case SpecialCharRegex() => Vector(("special_character", value), (feature, value))
          case _                  => Vector((feature, value))
        }
    }.seq.flatten
  }

}

class LemmaFeatureExtender extends FeatureExtender[String, String] {

  val lemmatizables = Set("verb", "noun")
  def extendFeatures(features: Vector[(String, String)]): Vector[(String, String)] = {
    features.par.map {
      case (feature, value) =>
        if (lemmatizables.contains(feature))
          Vector((feature + "_lemma", Lemmatize(value)), (feature, value))
        else
          Vector((feature, value))
    }.seq.flatten
  }

}

class AllTwoValueCombinationsFeatureExtender extends FeatureExtender[String, String] {

  def extendFeatures(features: Vector[(String, String)]): Vector[(String, String)] = {
    val newFeatures = for (x <- features.par; y <- features.par) yield (x._1 + y._1, x._2 + y._2)
    features ++ newFeatures
  }

}

class CapitalizationFeatureExtender extends FeatureExtender[String, String] {

  val CapitalsRegex = """[A-Z]+.*""".r
  def extendFeatures(features: Vector[(String, String)]): Vector[(String, String)] = {
    features.par.map {
      case (feature, value) =>
        value match {
          case CapitalsRegex() => Vector(("caps", value), (feature, value))
          case _               => Vector((feature, value))
        }
    }.seq.flatten
  }

}

class VNCombinationFeatureExtender extends FeatureExtender[String, String] {

  def extendFeatures(features: Vector[(String, String)]): Vector[(String, String)] = {
    features ++ Vector(
      ("verb+noun",
        features.filter(x => x._1 == "verb").toMap.getOrElse("verb", "") +
        "+" +
        features.filter(x => x._1 == "noun").toMap.getOrElse("noun", "")))
  }

}