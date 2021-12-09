package cz.fit.cvut
package SimulatedAnnealing

import scala.annotation.tailrec

case class Knapsack(id: Int, noItems: Int, size: Int, items: List[KnapsackItem])

class KnapsackOperator extends Operator[KnapsackState] {
  def switchNthItem(position: Int, state: KnapsackState): KnapsackState = {
    var a = state.items
    val changeValue = if (a(position).isPresented) -a(position).weight else a(position).weight
    if ((state.weight + changeValue) > state.knapsack.size) {
      state
    } else {
      a = a.updated(position, KnapsackItem(a(position).cost, a(position).weight, !a(position).isPresented))
      KnapsackState(a, state.knapsack)
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

case class KnapsackItem(cost: Int, weight: Int, isPresented: Boolean) extends Item

case class KnapsackState(items: List[KnapsackItem], knapsack: Knapsack) extends State[KnapsackState] {
  lazy val cost: Int = items.foldRight(0)((item, sum) => if (item.isPresented) sum + item.cost else sum)
  lazy val weight: Int = items.foldRight(0)((item, sum) => if (item.isPresented) sum + item.weight else sum)

  override def betterThan(that: KnapsackState): Boolean =
    if (this.cost == that.cost) this.weight <= that.weight else this.cost > that.cost

  override def howMuchWorstThan(that: KnapsackState): Int =
    math.abs(this.cost - that.cost)

  @tailrec
  private def commaSeparated(list: List[String], stringBuilder: StringBuilder): String = list match {
    case List() => ""
    case List(a) => (stringBuilder ++= a).toString()
    case h :: t => commaSeparated(t, (stringBuilder ++= h ++= " "))

  }

  override def toString: String = {
    commaSeparated(items.map(it => if (it.isPresented) "1" else "0"), new StringBuilder)
  }
}