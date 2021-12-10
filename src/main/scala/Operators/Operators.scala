package cz.fit.cvut
package Operators

import SimulatedAnnealing.State

import scala.util.Random

trait Operator[S <: State[S]] {
  def to(state: S): List[S]
}

case class Operators[S <: State[S]](ops: List[Operator[S]]) {
  private val randomGenerator = Random

  def random: Operator[S] = ops(randomGenerator.between(0, ops.size))
}