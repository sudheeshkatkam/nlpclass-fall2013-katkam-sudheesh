package nlp.a0

import io.Source

object WordFreqFreq {

  def main(args: Array[String]) : Unit = {
    args.length match{
      case 1 => val words = Source.fromFile(args(0)).mkString.toLowerCase.split("[^a-z]").groupBy(x=>x).mapValues(_.length) -- Set("")
	    		val sorted_words = words.groupBy(_._2).toVector.sortBy(_._1)
	    		println("Top 10 most frequent frequencies:")
	    		sorted_words.take(10).foreach{x => println(f"${x._2.size} " 
	    		+ f"${if (x._2.size == 1) "word appears" else "words appear"} "
	    		+ f"${x._1} ${if (x._1 == 1) "time" else "times"}")}
	    		
      			println("\nBottom 5 most frequent frequencies:")
    		    sorted_words.takeRight(5).foreach{x => println(f"${x._2.size} " 
	    		+ f"${if (x._2.size == 1) "word appears" else "words appear"} "
	    		+ f"${x._1} ${if (x._1 == 1) "time" else "times"}")}
      	
      case _ => println("Wrong number of arguments.")
    }
  }
}