package nlp.a2

import dhg.util.FileUtil._
import dhg.util.CollectionUtil._
import nlpclass.NaiveBayesTrainerToImplement
import nlpclass.NoOpFeatureExtender

object InvalidPosLabel extends Exception { }
object MissingInstances extends Exception { }

object NaiveBayes {
  
  def main(args: Array[String]) : Unit = {
    try{
	    val argslist = args.toList
	    
	    val training_instances =
	      if(argslist.indexOf("--train") != -1)
			File(argslist(argslist.indexOf("--train") + 1)).readLines.map { line =>
			    val featurePairs :+ label = line.split(",").toVector
			    label.trim -> featurePairs.map(_.split("=").map(_.trim).toTuple2)
			}.toVector
	      else throw MissingInstances
		val testing_instances =
		  if(argslist.indexOf("--test") != -1)
			File(argslist(argslist.indexOf("--test") + 1)).readLines.map { line =>
			    val featurePairs :+ label = line.split(",").toVector
			    label.trim -> featurePairs.map(_.split("=").map(_.trim).toTuple2)
			}.toVector
	      else throw MissingInstances
		
		val positiveLabel = 
		  if (argslist.indexOf("--poslab") != -1) 
		    argslist(argslist.indexOf("--poslab") + 1)
		  else throw InvalidPosLabel		
		val lambda = 
		  if (argslist.indexOf("--lambda") != -1) 
		    argslist(argslist.indexOf("--lambda") + 1).toDouble 
		  else 0.0
		val log = 
		  if (argslist.indexOf("--log") != -1) 
		    argslist(argslist.indexOf("--log") + 1).toBoolean
		  else false
		val extend = 
		  if (argslist.indexOf("--extend") != -1)
		    argslist(argslist.indexOf("--extend") + 1).toBoolean
		  else false
		
		val nbt = trainer(lambda, log, extend)
		val nbm = nbt.train(training_instances)
		NaiveBayesScorer.score(nbm, testing_instances, positiveLabel)
    
    } catch {
      case InvalidPosLabel => println("Missing positive label.")
      case MissingInstances => println("Missing test/train instances.")
    }
  
  }
  
  def trainer(lambda: Double, log: Boolean, extend: Boolean): NaiveBayesTrainerToImplement[String, String, String] = {
    if(lambda > 0 && log)
      return new LogAddLambdaNaiveBayesTrainer[String, String, String](lambda)    
    else if (lambda > 0)
      return new AddLambdaNaiveBayesTrainer[String, String, String](lambda)
    else
      return new UnsmoothedNaiveBayesTrainer[String, String, String]
  }
  
}