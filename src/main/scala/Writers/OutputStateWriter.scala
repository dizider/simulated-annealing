package cz.fit.cvut
package Writers

import SimulatedAnnealing.State

import monix.reactive.Consumer

trait OutputStateWriter[S <: State[S]] {
  def println(state: S): Consumer[(S, _), Unit]
}