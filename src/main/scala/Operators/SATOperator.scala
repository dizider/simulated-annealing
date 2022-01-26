package cz.fit.cvut
package Operators

import SimulatedAnnealing.{RawSATLiteral, SATState}

class SATOperator(implicit config: Config) extends Operator[SATState] {

  private val random = TimeBasedRandom

  def switchNthItem(position: Int, state: SATState): SATState = {
    var a = state.literalsValues
    a = a.updated(position, RawSATLiteral(a(position).cost, !a(position).isPresented))

    val newState = SATState(state.clauses, a, state.randomStrategyFactory)

    if (newState.cntSatisfiableClauses >= state.cntSatisfiableClauses) {
      newState
    } else {
      if (config.relaxationCoefficient >= random.nextDouble) {
        newState
      } else {
        state
      }
    }
  }

  override def to(state: SATState): List[SATState] = {
    var neighbours: Set[SATState] = Set.empty
    for (i <- Seq.range(1, state.literalsValues.size)) {
      neighbours = neighbours + switchNthItem(i, state)
    }
    neighbours.toList
  }
}

object SATOperator {
  def apply()(implicit config: Config): SATOperator = {
    new SATOperator
  }
}
