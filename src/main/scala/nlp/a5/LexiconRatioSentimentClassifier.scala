package nlp.a5

import nlpclass.Classifier
import com.typesafe.scalalogging.log4j.Logging

class LexiconRatioSentimentClassifier(posWords: Set[String], negWords: Set[String])
  extends Classifier[String, String, String] with Logging {

  def predict(features: Vector[(String, String)]): String = {
    val numPosWords = features.count { case (f, v) => posWords.contains(v) }
    val numNegWords = features.count { case (f, v) => negWords.contains(v) }

    logger.debug("features: " + features +
      "\n# Positive Words: " + numPosWords +
      "\n# Negative Words: " + numNegWords)

    if (numPosWords > numNegWords)
      "positive"
    else if (numPosWords < numNegWords)
      "negative"
    else
      "neutral"
  }

}