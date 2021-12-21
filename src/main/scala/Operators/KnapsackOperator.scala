package cz.fit.cvut
package Operators

import SimulatedAnnealing.{KnapsackItem, KnapsackState}

import scala.annotation.tailrec

class KnapsackOperator extends Operator[KnapsackState] {
  def switchNthItem(position: Int, state: KnapsackState): KnapsackState = {
    var a = state.items
    val changeValue = if (a(position).isPresented) -a(position).weight else a(position).weight
    if ((state.weight + changeValue) > state.knapsack.size) {
      state
    } else {
      a = a.updated(position, KnapsackItem(a(position).cost, a(position).weight, !a(position).isPresented))
      KnapsackState(a, state.knapsack, state.randomStrategyFactory)
    }
  }

  override def to(state: KnapsackState): List[KnapsackState] = {
    @tailrec
    def addNeighbor(position: Int, neighbors: Set[KnapsackState])(size: Int): List[KnapsackState] = {
      if (position >= size) neighbors.toList
      else addNeighbor(position + 1, neighbors + switchNthItem(position, state))(size)
    }

    addNeighbor(0, Set())(state.items.size)
  }
}

object KnapsackOperator {
  def apply(): KnapsackOperator = {
    new KnapsackOperator
  }
}
