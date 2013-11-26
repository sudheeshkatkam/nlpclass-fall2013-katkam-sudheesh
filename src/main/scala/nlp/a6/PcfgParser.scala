package nlp.a6

import nlpclass.Parser
import nlpclass.Tree

class PcfgParser extends Parser {

  def likelihood(t: Tree): Double = {
    // your code here
    ???
  }

  def parse(tokens: Vector[String]): Option[Tree] = ???
  def generate(): Tree = ???

}