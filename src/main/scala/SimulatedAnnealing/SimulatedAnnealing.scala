package cz.fit.cvut
package SimulatedAnnealing

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

trait Operator[S <: State[S]] {
  def to(state: S): List[S]
}

case class Operators[S <: State[S]](ops: List[Operator[S]]) {
  private val randomGenerator = {
    val r = Random
    r.setSeed(System.currentTimeMillis())
    r
  }

  def random: Operator[S] = ops(randomGenerator.between(0, ops.size))
}

case class Temperature(value: Double)

class SimulatedAnnealing[A <: State[A]](val operators: Operators[A]) {

  private val random = Random

  @tailrec
  final def solve(temperature: Temperature, state: A, best: A)(frozen: (Temperature => Boolean))(equilibrium: (Double => Boolean))(cool: (Temperature => Temperature)): A = {
    if (frozen(temperature)) best
    else {
      val innerLoopRes = innerLoop(state, best, 0)(temperature)(equilibrium)
      solve(cool(temperature), innerLoopRes._1, innerLoopRes._2)(frozen)(equilibrium)(cool)
    }
  }

  @tailrec
  private def innerLoop(state: A, best: A, counter: Int)(temperature: Temperature)(equilibrium: Double => Boolean): (A, A) = {
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
    // select random new state
    val newState = possibleNewStates(random.between(0, possibleNewStates.size))
    if (newState betterThan state) {
      newState // new state is better
    } else {
      val delta: Double = newState howMuchWorstThan state
      val y = math.exp(-delta / temperature.value)
      if (random.nextDouble() < y) newState else state
    }
  }
}