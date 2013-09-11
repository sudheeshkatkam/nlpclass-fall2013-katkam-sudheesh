package nlp.a0

import io.Source
import dhg.util.FileUtil._

object InvalidArg extends Exception { }
object WrongNumberOfArguments extends Exception { }

object WordCount {

  def main(args: Array[String]) : Unit = {
    try{ 
      args.length match {
	    case 1 => val words = Source.fromFile(args(0)).mkString.toLowerCase.split("[^a-z]").groupBy(x=>x).mapValues(_.length) -- Set("")
	    		  val sorted_words = words.keys.toVector.sortBy(x => words(x)).reverse
	    		  val total_num = words.values.sum
	    		  
	    		  println ("Total number of words: " + total_num)
	    		  println ("Number of distinct words: " + sorted_words.length)
	    		  println ("Top 10 words:")
	    		  (0 to 9).foreach (x => println (f"${sorted_words(x)}%-15s${words(sorted_words(x))}%-8s${((words(sorted_words(x)).doubleValue)/total_num)*100}%.2f"))
	    
	    case 3 => if (!(args(1) == "--stopwords")) { throw InvalidArg }
	    		  val raw_words = Source.fromFile(args(0)).mkString.toLowerCase.split("[^a-z]").groupBy(x=>x).mapValues(_.length) -- Set("")
	    		  val stop_words = Source.fromFile(args(2)).mkString.split("[^a-z]").toSet
	    		  val words = raw_words -- stop_words
	    		  val sorted_words = words.keys.toVector.sortBy(x => words(x)).reverse
	    		  val total_num = raw_words.values.sum
	    		  
	    		  println ("Total number of words: " + total_num)
	    		  println ("Number of distinct words: " + raw_words.keys.size)
	    		  println ("Top 10 words:")
	    		  (0 to 9).foreach (x => println (f"${sorted_words(x)}%-15s${words(sorted_words(x))}%-8s${((words(sorted_words(x)).doubleValue)/total_num)*100}%.2f"))
	    		  
	    case _ => throw WrongNumberOfArguments
	    }
	  } catch {
	    case InvalidArg => println ("Invalid arguments.")
	    case WrongNumberOfArguments => println ("Wrong number of arguments.")
	  }
  }
}