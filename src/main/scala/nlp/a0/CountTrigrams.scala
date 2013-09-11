package nlp.a0

import io.Source

object CountTrigrams {
  
  def main(args: Array[String]) : Unit = {
    val text_vector =  Source.fromFile(args(0)).mkString.toLowerCase.split("[^a-z]").toVector.filter(x => x.nonEmpty)
    val ngram_map = new nlp.a0.NGramCounting(3).countNGrams(text_vector)
    val sorted_by_values = ngram_map.toVector.sortBy(x => -x._2)
    sorted_by_values.take(10).foreach{x => println(f"${x._1.mkString(" ")}%-32s ${x._2}")}
  }
}