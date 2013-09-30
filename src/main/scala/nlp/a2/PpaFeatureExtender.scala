package nlp.a2

import nlpclass.FeatureExtender
import nlpclass.CompositeFeatureExtender
import nlpclass.Lemmatize

class PpaFeatureExtender[Feature, Value] extends FeatureExtender[Feature, Value] {

  def extendFeatures(features: Vector[(Feature, Value)]): Vector[(Feature, Value)] = {
    /*new CompositeFeatureExtender[Feature, Value](Vector(
      new NumberFeatureExtender(),
      new LemmaFeatureExtender(),
      new WordShapeFeatureExtender(),
      new VNCombinationFeatureExtender(),
      new CapitalizationFeatureExtender()))
    .extendFeatures(features)
    * 
    */
    new LemmaFeatureExtender[Feature, Value].extendFeatures(features)
  }

}

class NumberFeatureExtender[Feature, Value] extends FeatureExtender[Feature, Value] {

  def extendFeatures(features: Vector[(Feature, Value)]): Vector[(Feature, Value)] = {
    ???
  }

}

class LemmaFeatureExtender[Feature, Value] extends FeatureExtender[Feature, Value] {

  def extendFeatures(features: Vector[(Feature, Value)]): Vector[(Feature, Value)] = {
    features.map { case (f, v) => (f, Lemmatize(v.toString)) }
    ???
  }

}

class WordShapeFeatureExtender[Feature, Value] extends FeatureExtender[Feature, Value] {

  def extendFeatures(features: Vector[(Feature, Value)]): Vector[(Feature, Value)] = {
    ???
  }

}

class VNCombinationFeatureExtender[Feature, Value] extends FeatureExtender[Feature, Value] {

  def extendFeatures(features: Vector[(Feature, Value)]): Vector[(Feature, Value)] = {
    ???
  }

}

class CapitalizationFeatureExtender[Feature, Value] extends FeatureExtender[Feature, Value] {

  def extendFeatures(features: Vector[(Feature, Value)]): Vector[(Feature, Value)] = {
    ???
  }

}

class SpecialCharacterFeatureExtender[Feature, Value] extends FeatureExtender[Feature, Value] {

  def extendFeatures(features: Vector[(Feature, Value)]): Vector[(Feature, Value)] = {
    ???
  }

}