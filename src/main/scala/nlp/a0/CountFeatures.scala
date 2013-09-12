package nlp.a0

import io.Source

object CountFeatures {
  
  def main(args: Array[String]) : Unit = {
    // "A=B, C=D, ... L"
    // Map[String, Map [String, Map [String, Int]]]
    //     feature      label        value   count
    /*Source.fromFile(args(0)).getLines.toVector.map(line => 
      (line.split(",").toVector.last,line.split(",").toVector.dropRight(1).map(x => 
        x.split("=")).map(x => 
          (x(0), x(1))).toMap)).groupBy(x => 
            x._1).unzip._2.map(x => 
            x.map{ case(a,b) => 
              b.map{ case(k,v) => 
              (k,(a,v))}}.flatten).flatten.toVector.groupBy(x => 
                x).mapValues(_.size).groupBy{x => 
                  x._1._1}.toMap.map{ case(k,v) => 
                    (k, v.toList.map{ x => 
                      (x._1._2, x._2)}.toMap.groupBy{a => 
                        a._1._1}.toMap.map{ case(m,n) => 
                          (m, n.toList.map{ p => 
                            (p._1._2, p._2)}.toMap)})}.toVector.sortBy(_._1).foreach{ x => 
                              print(x._1); x._2.toVector.sortBy(_._1).foreach{ y => 
                                print("\n    " + y._1); y._2.toVector.sortBy(_._1).foreach{z => 
                                  print(f"\n${z._1}%8s" + f"${z._2}%24s")}; println()}} */

    val map_label_featurevaluepair = Source.fromFile(args(0)).getLines.toVector.map(
        handle1).groupBy(x => x._1)
    map_label_featurevaluepair.mapValues(x => x.size).foreach{x => println(x._1 + " " + x._2)}
    val vector_feature_labelvaluepairs = map_label_featurevaluepair.unzip._2.map(
        x => x.map{ case(a,b) => b.map{
          case(k,v) => (k,(a,v))}}.flatten).flatten.toVector
    val map_feature_labelvalue_count = vector_feature_labelvaluepairs.groupBy(x => x).mapValues(_.size)
    val map_feature_label_value_count = map_feature_labelvalue_count.groupBy{x => x._1._1}.toMap.map{
      case(k,v) => (k, v.toList.map{
        x => (x._1._2, x._2)}.toMap.groupBy{a => a._1._1}.toMap.map{
        case(m,n) => (m, n.toList.map{
          p => (p._1._2, p._2)}.toMap)
      })}
    map_feature_label_value_count.toVector.sortBy(_._1).foreach{
      x => print(x._1); x._2.toVector.sortBy(_._1).foreach{
        y => print("\n    " + y._1); y._2.toVector.sortBy(_._1).foreach{z => print(f"\n${z._1}%8s" + f"${z._2}%24s")}; println()}}

  }

  def handle1(line: String): Tuple2[String, Vector[Tuple2[String,String]]] = {
    // A=B, C=D, .. L1 
    // [ Tuple(L1, Map[(A, B), (C, D), ...]) ...]
    
    val fields = line.split(",").toVector
    val label = fields.last
    val rest = fields.dropRight(1).map(x => x.split("=")).map(x => (x(0), x(1))).toVector
    (label, rest)
  }
}