package cz.fit.cvut
package SimulatedAnnealing

import cz.fit.cvut.Operators.{KnapsackOperator, Operators}

import scala.annotation.tailrec
import scala.util.Random

case class Knapsack(id: Int, noItems: Int, size: Int, items: List[KnapsackItem])

case class KnapsackItem(cost: Int, weight: Int, isPresented: Boolean) extends Item

case class KnapsackState(items: List[KnapsackItem], knapsack: Knapsack) extends State[KnapsackState] {
  lazy val cost: Int = items.foldRight(0)((item, sum) => if (item.isPresented) sum + item.cost else sum)
  lazy val weight: Int = items.foldRight(0)((item, sum) => if (item.isPresented) sum + item.weight else sum)

  private def randomIsPresented: Boolean = {
    if (Random.nextDouble() < 0.5) false
    else true
  }

  @tailrec
  final def shuffle: KnapsackState = {
    val random = KnapsackState(this.items.map(it => KnapsackItem(it.cost, it.weight, randomIsPresented)), this.knapsack)
    if (random.weight > this.knapsack.size) shuffle
    else random
  }

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

class KnapsackSolver extends SimulatedAnnealing[KnapsackState](Operators(List(KnapsackOperator())))

object KnapsackSolver {
  def apply(): KnapsackSolver = new KnapsackSolver()
}