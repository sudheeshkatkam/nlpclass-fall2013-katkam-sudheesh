package nlp.a2

import nlpclass.FeatureExtender
import nlpclass.CompositeFeatureExtender
import nlpclass.Lemmatize

class PpaFeatureExtender[Feature, Value] extends FeatureExtender[Feature, Value]{
  
  def extendFeatures(features: Vector[(Feature, Value)]): Vector[(Feature, Value)] = {
    val feature_extender = new CompositeFeatureExtender[Feature, Value](Vector(
    new NumberFeatureExtender(),
    new LemmaFeatureExtender(),
    new WordShapeFeatureExtender(),
    new VNCombinationFeatureExtender(),
    new CapitalizationFeatureExtender()))
    feature_extender.extendFeatures(features)
  }
  
}

class NumberFeatureExtender[Feature, Value] extends FeatureExtender[Feature, Value]{

  def extendFeatures(features: Vector[(Feature, Value)]): Vector[(Feature, Value)] = {
    ???
  }

}

class LemmaFeatureExtender[Feature, Value] extends FeatureExtender[Feature, Value]{

  def extendFeatures(features: Vector[(Feature, Value)]): Vector[(Feature, Value)] = {
    ???
  }

}

class WordShapeFeatureExtender[Feature, Value] extends FeatureExtender[Feature, Value]{

  def extendFeatures(features: Vector[(Feature, Value)]): Vector[(Feature, Value)] = {
    ???
  }

}

class VNCombinationFeatureExtender[Feature, Value] extends FeatureExtender[Feature, Value]{

  def extendFeatures(features: Vector[(Feature, Value)]): Vector[(Feature, Value)] = {
    ???
  }

}

class CapitalizationFeatureExtender[Feature, Value] extends FeatureExtender[Feature, Value]{

  def extendFeatures(features: Vector[(Feature, Value)]): Vector[(Feature, Value)] = {
    ???
  }

}