package nlp.a0

import io.Source

object CountFeatures {
  
  def main(args: Array[String]) : Unit = {
    // "A=B, C=D, ... L"
    // Map[String, Map [String, Map [String, Int]]]
    //     feature      label        value   count
    val lines =  Source.fromFile(args(0)).getLines.toVector
    println (lines.map(handle).groupBy(x => x._1).mapValues(x => x))
  }
  
  def handle(line: String): Tuple2[String, Vector[Tuple2[String,String]]] = {
    // A=B, C=D, .. L1 
    // [ (L1, [(A, B), (C, D), ...]) ...]
    val fields = line.split(",").toVector
    val label = fields.last
    val rest = fields.dropRight(1).map(x => x.split("=")).map(x => (x(0), x(1)))
    (label, rest)
  }
}