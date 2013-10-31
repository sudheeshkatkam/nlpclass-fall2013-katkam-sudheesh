package nlp.a4

import nlpclass.ConditionalProbabilityDistributionToImplement
import com.typesafe.scalalogging.log4j.Logging

object Viterbi extends Logging {

  protected class Node[S](val prev: Node[S], val score: Double, val state: S) {
    override def toString = { f"(v:${score}%.4f, s:" + state.toString + ")" }
  }

  def apply[O, S](states: Set[S],
                  startState: S,
                  endState: S,
                  sequence: Vector[O],
                  emissionProbs: ConditionalProbabilityDistributionToImplement[S, O],
                  transitionProbs: ConditionalProbabilityDistributionToImplement[S, S]): Vector[S] = {

    //forward pass
    val matrix = forward((states - startState - endState).toVector, startState, endState, sequence, emissionProbs, transitionProbs)

//    logger.info(matrix.map { x => ": " + x })
//    logger.info("matrix length: " + matrix.size)

    //backward pass
    val path = backward(matrix)

//    logger.info("VITERBI PATH:" + path)
    path
  }

  private def forward[O, S](states: Vector[S],
                            startState: S,
                            endState: S,
                            sequence: Vector[O],
                            emissionProbs: ConditionalProbabilityDistributionToImplement[S, O],
                            transitionProbs: ConditionalProbabilityDistributionToImplement[S, S]): Vector[Vector[Node[S]]] = {
    val matrix = sequence.drop(1).dropRight(1).scanLeft(Vector.fill(1)(new Node[S](null, 1.0, startState))) {
      (acc, observation) =>
        val scores = states.map {
          state =>
            val prevNodes = acc
            val prob = emissionProbs(observation, state)

//            logger.debug("P(" + observation + "|" + state + f"): ${prob}%.4f")

            val best = score[S](state, prevNodes, transitionProbs)
            new Node[S](best._1, prob * best._2, state)
        }
        scores
    }

//    logger.debug("P(" + sequence.last + "|" + endState + f"): ${emissionProbs(sequence.last, endState)}%.4f")

    val endScore = score[S](endState, matrix(sequence.size - 2), transitionProbs)

//    logger.info("BEST PREVIOUS: " + endScore._1 + f" score: ${endScore._2}%.4f")
    val end = new Node[S](endScore._1, emissionProbs(sequence.last, endState) * endScore._2, endState)
    matrix :+ Vector(end)
  }

  private def score[S](state: S,
                       prevNodes: Vector[Node[S]],
                       transitionProbs: ConditionalProbabilityDistributionToImplement[S, S]): (Node[S], Double) = {
    val best = prevNodes.map {
      prev =>
//        logger.debug("\t Prev: " + prev +
//          f" prob(${state}|${prev.state}): ${transitionProbs(state, prev.state)}%.4f" +
//          f" score: ${prev.score * transitionProbs(state, prev.state)}%.4f")

        (prev, prev.score * transitionProbs(state, prev.state))
    }.maxBy(_._2)

//    logger.info("BEST PREVIOUS: " + best._1 + f" score: ${best._2}%.4f")
    best
  }

  private def backward[S](matrix: Vector[Vector[Node[S]]]): Vector[S] = {
    val end = matrix.last.head

    val nodes = matrix.dropRight(1).foldLeft(Vector(end)) {
      (acc, vector) =>
        acc.head.prev +: acc
    }

    nodes.map { node => node.state }
  }

}