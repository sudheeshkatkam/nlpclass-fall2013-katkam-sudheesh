package nlp.a0

import io.Source

object CountFeatures {
  
  def main(args: Array[String]) : Unit = {
    // "A=B, C=D, ... L"
    // Map[String, Map [String, Map [String, Int]]]
    //     feature      label        value   count
    val lines =  Source.fromFile(args(0)).getLines.toVector
    lines.map(handle).groupBy(x => x._1).mapValues(x => x.size).foreach{x => println(x._1 + " " + x._2)}
    println()
    lines.map(handle).groupBy(x => x._2).foreach{x => println("key: " + x._1 + "\nvalue: " + x._2)}
  }
  
  def handle(line: String): Tuple2[String, Map[String,String]] = {
    // A=B, C=D, .. L1 
    // [ Tuple(L1, Map[(A, B), (C, D), ...]) ...]
    val fields = line.split(",").toVector
    val label = fields.last
    val rest = fields.dropRight(1).map(x => x.split("=")).map(x => (x(0), x(1))).toMap
    (label, rest)
  }
}