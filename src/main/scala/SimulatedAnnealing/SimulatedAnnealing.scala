package cz.fit.cvut
package SimulatedAnnealing

import Operators._

import cats.effect.Concurrent
import cats.implicits._
import cz.fit.cvut.Writers.OutputWriter

import java.io.FileWriter
import scala.annotation.tailrec
import scala.language.implicitConversions

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

case class StateOfComputation[A <: State[A]](temperature: Temperature, state: A, best: A, iteration: Int = 0, counter: Int)

class SimulatedAnnealing[F[_], A <: State[A]](val operators: Operators[A], val randomStrategyFactory: RandomStrategyFactory, writer: Option[OutputWriter[F]])(implicit config: Config, F: Concurrent[F]) {

  type ActState = A
  type BestState = A
  type StatePair = (ActState, BestState) // actState, bestState

  private lazy val random: CustomRandom = randomStrategyFactory.get()

  //  val source: Option[FileWriter] = config.partialSolutionOutput.map(f => new FileWriter(f.getPath))

  def solve(temperature: Temperature, state: A, best: A, iteration: Int = 0)(implicit frozen: (Temperature => Boolean), equilibrium: Double, cool: (Temperature => Temperature)): F[A] = {
    fs2.Stream.eval(innerSolver(temperature, state, best, iteration)).compile.lastOrError
  }

  private final def innerSolver(temperature: Temperature, state: A, best: A, iteration: Int = 0)(implicit frozen: (Temperature => Boolean), equilibrium: Double, cool: (Temperature => Temperature)): F[A] = {
    if (frozen(temperature)) {
      F.pure(best)
    }
    else {
      innerLoop(state, best)(iteration, temperature, if (temperature.value > 100) equilibrium else equilibrium).flatMap(innerLoopRes =>
        innerSolver(cool(temperature), innerLoopRes._1, innerLoopRes._2, iteration + 1)(frozen, equilibrium, cool))
    }
  }

  private def innerLoop(state: A, best: A, counter: Int = 0, accepted: Int = 0)(implicit iteration: Int, temperature: Temperature, equilibrium: Double): F[StatePair] = {
    for {
      _ <- writer.map { a => a.write(s"${iteration+counter} ${state.toString}") }.getOrElse(F.unit)
      a <-
        if (counter > equilibrium) {
          F.pure((state, best))
        } else {
          val newState = tr(state, temperature)
          if ((newState betterThan best) && newState.isValid) {
            innerLoop(newState, newState, counter + 1)
          } else {
            innerLoop(newState, best, counter + 1)
          }
        }
    } yield a

    //    source.foreach(_.write(s"${(iteration * equilibrium) + counter} ${state.valueOfOptimization()} ${best.valueOfOptimization()} ${state.isValid}\n"))
    //    println(s"${(iteration * equilibrium) + counter} ${state.valueOfOptimization()} ${best.valueOfOptimization()} ${state.isValid}")
  }

  private def tr(state: A, temperature: Temperature): A = {
    val possibleNewStates = operators.random to state
    // selects a new state at random from a set of neighbors
    val newState = possibleNewStates(random.intInRange(0, possibleNewStates.size))

    if (newState betterThan state) {
      newState
    } else {
      val delta: Double = newState howMuchWorstThan state
      val y = math.exp(-delta / temperature.value) // accepts worse state with this probability
      if (random.nextDouble < y) {
        newState
      } else {
        state
      }
    }
  }
}