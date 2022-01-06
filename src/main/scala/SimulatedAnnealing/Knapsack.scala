package cz.fit.cvut
package SimulatedAnnealing

import Operators.{KnapsackOperator, Operators}

import java.io.File
import scala.annotation.tailrec

case class KnapsackSolverFactory()(implicit config: Config) {
  def withTimeBasedRandom: KnapsackSolver = {
    KnapsackSolver(TimeBasedRandomStrategy())
  }
}

case class Knapsack(id: Int, noItems: Int, size: Int, items: List[KnapsackItem])

case class KnapsackItem(cost: Double, weight: Int, isPresented: Boolean) extends Item

case class KnapsackState(items: List[KnapsackItem], knapsack: Knapsack, randomStrategyFactory: RandomStrategyFactory) extends State[KnapsackState] {
  private lazy val random: CustomRandom = randomStrategyFactory.get()

  lazy val cost: Double = items.foldRight(0.0)((item, sum) => if (item.isPresented) sum + item.cost else sum)
  lazy val weight: Int = items.foldRight(0)((item, sum) => if (item.isPresented) sum + item.weight else sum)

  private def randomIsPresented: Boolean = {
    if (random.nextDouble < 0.5) false
    else true
  }

  @tailrec
  final def shuffle: KnapsackState = {
    val random = KnapsackState(this.items.map(it => KnapsackItem(it.cost, it.weight, randomIsPresented)), this.knapsack, this.randomStrategyFactory)
    if (random.weight > this.knapsack.size) shuffle
    else random
  }

  override def betterThan(that: KnapsackState): Boolean =
    if (this.cost == that.cost) this.weight <= that.weight else this.cost > that.cost

  override def howMuchWorstThan(that: KnapsackState): Double =
    math.abs(this.cost - that.cost)

  @tailrec
  private def commaSeparated(list: List[String], stringBuilder: StringBuilder): String = list match {
    case List() => ""
    case List(a) => (stringBuilder ++= a).toString()
    case h :: t => commaSeparated(t, (stringBuilder ++= h ++= " "))

  }

  override def toString: String = {
    s"${knapsack.id} ${knapsack.noItems} $cost ${commaSeparated(items.map(it => if (it.isPresented) "1" else "0"), new StringBuilder)}"
  }

  override def valueOfOptimization(): Double = cost
}

class KnapsackSolver(randomStrategyFactory: RandomStrategyFactory)(implicit config: Config) extends SimulatedAnnealing[KnapsackState](Operators(List(KnapsackOperator())), randomStrategyFactory)

object KnapsackSolver {
  def apply(randomStrategyFactory: RandomStrategyFactory)(implicit config: Config): KnapsackSolver = new KnapsackSolver(randomStrategyFactory)
}