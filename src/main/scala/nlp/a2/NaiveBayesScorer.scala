package nlp.a2

import nlpclass.NaiveBayesScorerToImplement
import nlpclass.NaiveBayesModelToImplement

object NaiveBayesScorer extends NaiveBayesScorerToImplement {

  def score[Label, Feature, Value](
    naiveBayesModel: NaiveBayesModelToImplement[Label, Feature, Value],
    testInstances: Vector[(Label, Vector[(Feature, Value)])],
    positiveLabel: Label) = {
    val predictions = testInstances.map {
      case (label, fvpairs) =>
        (label, naiveBayesModel.predict(fvpairs))
    }

    val accuracy = (predictions.count(x => x._1 == x._2).doubleValue / predictions.size) * 100
    println(f"accuracy = ${accuracy}%.2f")

    val tp = predictions.groupBy(_._2)(positiveLabel).count(x => x._1 == x._2)
    val fp = predictions.groupBy(_._2)(positiveLabel).count(x => x._1 != x._2)
    val fn = predictions.groupBy(_._1)(positiveLabel).count(x => x._1 != x._2)
    val precision = (tp.doubleValue / (tp + fp)) * 100
    val recall = (tp.doubleValue / (tp + fn)) * 100
    println(f"precision (${positiveLabel}) = ${precision}%.2f")
    println(f"recall (${positiveLabel}) = ${recall}%.2f")

    val f1 = (2 * precision * recall) / (precision + recall)
    println(f"f1 = ${f1}%.2f")
  }

}

//val fp = predictions.groupBy(_._2).foldLeft(0){ (acc, groupedByPred) => 
//  if(groupedByPred._1 != positiveLabel) acc + groupedByPred._2.size else acc + 0
//}
    