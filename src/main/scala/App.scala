package cz.fit.cvut

import Parsers.KnapsackParser
import SimulatedAnnealing._

import scala.io.Source
import scala.language.implicitConversions

class App {
}

object Main {

//  lazy val temperature =

  implicit val equilibrium: Double => Boolean = (number: Double) => {
    number > 50
  }

  implicit val cool: Temperature => Temperature = (temperature: Temperature) => {
    temperature * 0.995
  }

  implicit val frozen: Temperature => Boolean = (temperature: Temperature) => {
    temperature.value < 4.7
  }

  def main(array: Array[String]): Unit = {

    val knapsacks = new KnapsackParser(Source.fromFile(array(0))).parse()

    knapsacks.foreach { knapsack =>
      val initState: KnapsackState = KnapsackState(knapsack.items, knapsack).shuffle
      val solution = KnapsackSolver().solve(Temperature(47), initState, initState)
      println(s"${solution.knapsack.id} ${solution.cost} $solution")
    }
  }
}
