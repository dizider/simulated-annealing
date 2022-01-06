package cz.fit.cvut
package Writers

import SimulatedAnnealing.SATState

import monix.eval.Task

import java.io.FileWriter

class SATWriter(output: FileWriter) extends OutputStateWriter[SATState] {
  override def println(state: SATState): Task[Unit] = Task(output.write(s"${state}\n"))
}
