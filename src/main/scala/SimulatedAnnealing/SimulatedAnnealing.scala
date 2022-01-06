package cz.fit.cvut
package SimulatedAnnealing

import Operators._

import java.io.{File, FileWriter}
import scala.annotation.tailrec

trait NestedState[+A]

trait Item {
  def cost: Double

  def isPresented: Boolean
}

abstract class State[A] {
  lazy val isValid: Boolean = false

  def valueOfOptimization(): Double

  def betterThan(that: A): Boolean

  def howMuchWorstThan(that: A): Double
}

case class Temperature(value: Double) {
  def *(that: Double): Temperature = {
    Temperature(this.value * that)
  }

  def *(that: Temperature): Temperature = {
    Temperature(this.value * that.value)
  }
}

class SimulatedAnnealing[A <: State[A]](val operators: Operators[A], val randomStrategyFactory: RandomStrategyFactory)(implicit config: Config) {

  type StateBest = (A, A) // actState, bestState

  private lazy val random: CustomRandom = randomStrategyFactory.get()

  val source: Option[FileWriter] = config.partialSolutionOutput.map(f => new FileWriter(f.getPath))

  @tailrec
  final def solve(temperature: Temperature, state: A, best: A, iteration: Int = 0)(implicit frozen: (Temperature => Boolean), equilibrium: Double, cool: (Temperature => Temperature)): A = {
    if (frozen(temperature)) {
      source.foreach(_.close())
      best
    }
    else {
      val innerLoopRes = innerLoop(state, best)(iteration, temperature, equilibrium)
      solve(cool(temperature), innerLoopRes._1, innerLoopRes._2, iteration + 1)(frozen, equilibrium, cool)
    }
  }

  @tailrec
  private def innerLoop(state: A, best: A, counter: Int = 0, accepted: Int = 0)(implicit iteration: Int, temperature: Temperature, equilibrium: Double): StateBest = {
    source.foreach(_.write(s"${(iteration * equilibrium) + counter} ${state.valueOfOptimization()} ${best.valueOfOptimization()} ${state.isValid}\n"))
    //    println(s"${(iteration * equilibrium) + counter} ${state.valueOfOptimization()} ${best.valueOfOptimization()} ${state.isValid}")
    if (counter > equilibrium) {
      (state, best)
    } else {
      val newState = tr(state, temperature)
      val newAccepted = if (newState != state) accepted + 1 else accepted
      if ((newState betterThan best) && newState.isValid) {
        innerLoop(newState, newState, counter + 1, newAccepted)
      } else {
        innerLoop(newState, best, counter + 1, newAccepted)
      }
    }
  }

  private def tr(state: A, temperature: Temperature): A = {
    val possibleNewStates = operators.random to state
    // selects a new state at random from a set of neighbors
    val newState = possibleNewStates(random.intInRange(0, possibleNewStates.size))
    if (newState betterThan state) {
      //      println(s"${newState.valueOfOptimization()} is better then ${state.valueOfOptimization()}")
      newState // new state is better
    } else {
      val delta: Double = (newState howMuchWorstThan state)
      val y = math.exp(-delta / temperature.value) // accepts worse state with this probability
      if (random.nextDouble < y) {
        newState
      } else {
        state
      }
    }
  }
}