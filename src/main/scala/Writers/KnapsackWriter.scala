package cz.fit.cvut
package Writers


import SimulatedAnnealing.KnapsackState

import monix.eval.Task

import java.io.FileWriter

class KnapsackWriter(output: FileWriter) extends OutputStateWriter[KnapsackState] {
  def println(state: KnapsackState): Task[Unit] = Task(output.write(s"${state}\n"))
}

class KnapsackWriterBuilder {
  //  var knapsackWriter: Option[KnapsackWriter] = None
  //  var file: Option[File] = None
  //
  //  def writeTo(file: File): KnapsackWriterBuilder = {
  //    this.file = Some(file)
  //    new FileWriter(file, false).write("")
  //    this
  //  }
  //
  //  def build: Either[String, KnapsackWriter] = {
  //    file.map(f => new KnapsackWriter(new FileWriter(f, true))).toRight("Knapsack Writer Build Unsuccessful")
  //  }
}

