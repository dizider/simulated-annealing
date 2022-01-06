package cz.fit.cvut
package Operators

import SimulatedAnnealing.{RawSATLiteral, SATState}

import scala.annotation.tailrec

class SATOperator(implicit config: Config) extends Operator[SATState] {

  private val random = TimeBasedRandom

  def switchNthItem(position: Int, state: SATState): SATState = {
    var a = state.literalsValues
    a = a.updated(position, RawSATLiteral(a(position).cost, !a(position).isPresented))

    val newClauses = state.clauses.map(clause => clause.updateLiterals(a))
    val newState = SATState(newClauses, a, state.randomStrategyFactory)

    if (newState.cntSatisfiableClauses >= state.cntSatisfiableClauses) {
      newState
    } else {
      if (config.relaxationCoefficient * 100 >= random.intInRange(0, 100)) {
        newState
      } else {
        state
      }
    }
  }

  override def to(state: SATState): List[SATState] = {
    @tailrec
    def addNeighbor(position: Int, neighbors: Set[SATState])(size: Int): List[SATState] = {
      if (position >= size) neighbors.toList
      else addNeighbor(position + 1, neighbors + switchNthItem(position, state))(size)
    }

    addNeighbor(1, Set(state))(state.literalsValues.size)
  }
}

object SATOperator {
  def apply()(implicit config: Config): SATOperator = {
    new SATOperator
  }
}
