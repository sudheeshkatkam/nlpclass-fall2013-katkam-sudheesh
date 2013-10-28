package nlp.a4

import nlpclass.HiddenMarkovModelToImplement

object ModelEvaluator {

  def apply[Word, Tag](model: HiddenMarkovModelToImplement[Word, Tag],
                       testSentences: Vector[Vector[(Word, Tag)]]) = {
    val labels = testSentences.map {
      sentence =>
        val words  = sentence.map(_._1)
        val gold   = sentence.map(_._2)
        val output = model.tagSentence(words)
        output.zip(gold)
    }.flatten

    val accuracy = labels.foldLeft((0, 0)) {
      (acc, label) =>
        val (guess, actual) = label
        (if (guess == actual) acc._1 + 1 else acc._1, acc._2 + 1)
    }

    val filtered = labels.filter { case (guess, actual) => guess != actual }

    val grouped = filtered.groupBy(_._1).map {
      case (output, groupedByOutput) =>
        groupedByOutput.groupBy(_._2).map {
          case (actual, groupedByActual) =>
            (output, actual, groupedByActual.size)
        }
    }.flatten.toVector

    val sorted = grouped.sortBy(-_._3).take(10)

    println(f"Accuracy: ${(accuracy._1.toDouble / accuracy._2) * 100}%.2f  (${accuracy._1}/${accuracy._2})")
    println("count  gold  model")
    sorted.map {
      case (output, actual, size) =>
        println(f"${size}%5s${actual}%6s${output}%7s")
    }
  }

}

//val accuracy = testSentences.foldLeft((0,0)){
//  (acc, sentence) =>
//    val words = sentence.map(_._1)
//    val gold = sentence.map(_._2)
//    val output = model.tagSentence(words)
//    val count = output.zip(gold).count { case (guess, actual) => guess == actual }
//    (acc._1 + count, acc._2 + gold.size)
//}