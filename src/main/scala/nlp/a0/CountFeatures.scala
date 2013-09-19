package nlp.a0

import io.Source
import dhg.util.FileUtil._
import dhg.util.CollectionUtil._

object CountFeatures {

	/*def main(args: Array[String]) : Unit = {
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
    map_label_featurevaluepair.mapValues(x => x.size).toVector.sorted.foreach{x => println(x._1 + " " + x._2)}
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
	 *
	 */

	def main(args: Array[String]): Unit = {
			val Seq(filename) = args.toList

			/*
			 * Count all the things
			 */
			val instances =
			File(filename).readLines.map { line =>
				val featurePairs :+ label = line.split(",").toVector // split into feature pairs and a final label
				label.trim -> featurePairs.map(_.split("=").map(_.trim).toTuple2) // turn feature pairs into tuples
			}.toVector
			val labelCounts = instances.map { case (label, _) => label }.counts // count just the labels
			val valueCountsByLabelByFeature = getCountsFunctional(instances)

			/*
			 * Print out label counts
			 */
			for ((label, count) <- labelCounts.toVector.sortBy(_._1))
				println(f"$label%-7s $count")
				println

				/*
				 * Print out feature value counts
				 */
				for ((feature, valueCountsByLabel) <- valueCountsByLabelByFeature.toVector.sortBy(_._1)) {
					println(f"$feature")
					for ((label, valueCounts) <- valueCountsByLabel.toVector.sortBy(_._1)) {
						println(f"    $label")
						for ((value, count) <- valueCounts.toVector.sortBy(_._1)) {
							println(f"        $value%-15s $count")
						}
					}
				}
	}

	def getCountsFunctional(instances: Vector[(String, Vector[(String, String)])]): Map[String, Map[String, Map[String, Int]]] = {
			/*
			 * Flatten the instances into (feature, label, value) triples
			 */
			val flvTriples =
					for (
							(label, fvPairs) <- instances;
							(feature, value) <- fvPairs
						) yield {
						(feature, label, value)
					}

			flvTriples.groupBy(_._1).map { // group by features
				case (feature, groupedByFeature) => // feature mapped to all triples with that feature
				feature ->
				groupedByFeature.groupBy(_._2).map { // group by label
					case (label, groupedByLabel) => // label mapped to all triples with that feature and label combo
					label ->
					groupedByLabel.groupBy(_._3).map { // group by value
						case (value, groupedByValue) => // value mapped to all triples with that feature, label, and value combo
						value -> groupedByValue.size // count the number of triples with that feature, label, and value combo
					}
				}
			}
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