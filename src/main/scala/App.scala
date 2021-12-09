package cz.fit.cvut

import Parsers.KnapsackParser
import SimulatedAnnealing._

import scala.io.Source

class App {
}

object Main {

  def equilibrium(number: Double): Boolean = {
    number > 50
  }

  def cool(temperature: Temperature): Temperature = {
    Temperature(temperature.value * 0.995)
  }

  def frozen(temperature: Temperature): Boolean = {
    temperature.value < 4.7
  }

  def main(array: Array[String]): Unit = {

    val knapsacks = new KnapsackParser(Source.fromFile(array(0))).parse()

    knapsacks.foreach { knapsack =>
      val op: Operators[KnapsackState] = Operators(List(KnapsackOperator()))
      val sa: SimulatedAnnealing[KnapsackState] = new SimulatedAnnealing(op)

      val initState: KnapsackState = KnapsackState(knapsack.items, knapsack)
      println(s"${initState.weight} $initState")

      val result = sa.solve(Temperature(47), initState, initState)(frozen)(equilibrium)(cool)
      println(s"${knapsack.id} ${result.cost} ${result.weight} $result")
    }
  }
}
