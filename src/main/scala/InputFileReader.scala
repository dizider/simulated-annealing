package cz.fit.cvut

import cats.effect.{Blocker, Concurrent, ContextShift}
import fs2.text

import java.nio.file.Paths

trait InputReader[F[_]] {
  def readLine(): F[Byte]
}

object InputFileReader {
  def create[F[_]](sourceFile: String)(implicit blocker: Blocker, F: Concurrent[F], cs: ContextShift[F]): fs2.Stream[F, String] = {

    def fromFile(fileName: String): fs2.Stream[F, String] = {
        fs2.io.file.readAll(Paths.get(fileName), blocker, 4096)
          .through(text.utf8Decode)
          .through(text.lines)
          .onFinalize(F.delay(println("Input file reached end")))
    }

    fromFile(sourceFile)
  }
}
