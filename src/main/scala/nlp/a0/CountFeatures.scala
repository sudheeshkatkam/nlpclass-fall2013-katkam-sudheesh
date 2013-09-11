package nlp.a0

import io.Source

object CountFeatures {
  
  def main(args: Array[String]) : Unit = {
    val lines =  Source.fromFile(args(0)).getLines
    lines.map(handle)
  }
  
  def handle(line: String): Vector[Tuple2[String, Vector[Tuple2[String,String]]]] = {
    // [(L1, [(A,B), (C, D), ...]) ...]
    
    ???
  }
}