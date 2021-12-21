package cz.fit.cvut

import SimulatedAnnealing.KnapsackState

class KnapsackError(val knapsacks: Map[Int, KnapsackState]) {
  def getError(that: KnapsackState): Option[Double] = {
    knapsacks.get(that.knapsack.id).map(knapsack => Math.abs(knapsack.cost - that.cost).toDouble / knapsack.cost.toDouble)
  }
}
