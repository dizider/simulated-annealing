package cz.fit.cvut
package Writers

import SimulatedAnnealing.KnapsackState

import java.io.{File, FileWriter}


class KnapsackWriter(file: FileWriter) extends OutputWriter[KnapsackState] {
  override def println(state: KnapsackState): Unit = {
    file.write(state.toString)
    file.close()
  }

  def println(state: String): Unit = {
    file.write(s"$state\n")
    file.close()
  }
}

class KnapsackWriterBuilder {
  var knapsackWriter: Option[KnapsackWriter] = None
  var file: Option[File] = None

  def writeTo(file: File): KnapsackWriterBuilder = {
    this.file = Some(file)
    new FileWriter(file, false).write("")
    this
  }

  def build: Either[String, KnapsackWriter] = {
    file.map(f => new KnapsackWriter(new FileWriter(f, true))).toRight("Knapsack Writer Build Unsuccessful")
  }
}

