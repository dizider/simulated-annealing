package cz.fit.cvut
package Writers

import monix.eval.Task

import java.io.FileWriter

trait OutputWriter {
  def println(string: String): Task[Unit]

  def print(string: String): Task[Unit]
}

class OutputFileWriter(val file: FileWriter) extends OutputWriter {
  override def println(string: String): Task[Unit] = Task(file.write(s"${string}\n"))

  override def print(string: String): Task[Unit] = Task(file.write(s"${string}"))
}
