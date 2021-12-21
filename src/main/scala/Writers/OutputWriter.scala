package cz.fit.cvut
package Writers

import SimulatedAnnealing.State

trait OutputWriter[S <: State[S]] {
  def println(state: S): Unit
}