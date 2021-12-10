package cz.fit.cvut
package SimulatedAnnealing

import cz.fit.cvut.Operators._

import scala.annotation.tailrec
import scala.util.Random

trait NestedSate[+A]

trait Item {
  def cost: Int

  def isPresented: Boolean
}

abstract class State[A] extends NestedSate[A] {
  def betterThan(that: A): Boolean

  def howMuchWorstThan(that: A): Int
}

case class Temperature(value: Double) {
  def *(that: Double): Temperature = {
    Temperature(this.value * that)
  }

  def *(that: Temperature): Temperature = {
    Temperature(this.value * that.value)
  }
}

class SimulatedAnnealing[A <: State[A]](val operators: Operators[A]) {

  type StateBest = (A, A) // actState, bestState

  private val random = Random

  @tailrec
  final def solve(temperature: Temperature, state: A, best: A)(implicit frozen: (Temperature => Boolean), equilibrium: (Double => Boolean), cool: (Temperature => Temperature)): A = {
    if (frozen(temperature)) best
    else {
      val innerLoopRes = innerLoop(state, best, 0)(temperature)(equilibrium)
      solve(cool(temperature), innerLoopRes._1, innerLoopRes._2)(frozen, equilibrium, cool)
    }
  }

  @tailrec
  private def innerLoop(state: A, best: A, counter: Int)(temperature: Temperature)(equilibrium: Double => Boolean): StateBest = {
    if (equilibrium(counter)) {
      (state, best)
    } else {
      val newState = tr(state, temperature)
      if (newState betterThan best) {
        innerLoop(newState, newState, counter + 1)(temperature)(equilibrium)
      } else {
        innerLoop(newState, best, counter + 1)(temperature)(equilibrium)
      }
    }
  }

  private def tr(state: A, temperature: Temperature): A = {
    val possibleNewStates = operators.random to state
    // selects a new state at random from a set of neighbors
    val newState = possibleNewStates(random.between(0, possibleNewStates.size))
    if (newState betterThan state) {
      newState // new state is better
    } else {
      val delta: Double = newState howMuchWorstThan state
      val y = math.exp(-delta / temperature.value) // accepts worse state with this probability
      if (random.nextDouble() < y) newState else state
    }
  }
}