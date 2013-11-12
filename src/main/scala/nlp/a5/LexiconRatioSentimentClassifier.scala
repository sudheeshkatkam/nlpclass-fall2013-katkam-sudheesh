package nlp.a5

import nlpclass.Classifier
import com.typesafe.scalalogging.log4j.Logging

class LexiconRatioSentimentClassifier(posWords: Set[String], negWords: Set[String])
  extends Classifier[String, String, String] with Logging {

  def predict(features: Vector[(String, String)]): String = {
    val numPWords = features.count { case (f, v) => posWords.contains(v) }
    val numNWords = features.count { case (f, v) => negWords.contains(v) }

    logger.debug("features: " + features +
      "\n# Positive Words: " + numPWords +
      "\n# Negative Words: " + numNWords)

    if (numPWords > numNWords)
      "positive"
    else if (numPWords < numNWords)
      "negative"
    else
      "neutral"
  }

}