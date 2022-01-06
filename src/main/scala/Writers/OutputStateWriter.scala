package cz.fit.cvut
package Writers

import SimulatedAnnealing.State

import monix.eval.Task

trait OutputStateWriter[S <: State[S]] {
  def println(state: S): Task[Unit]
}