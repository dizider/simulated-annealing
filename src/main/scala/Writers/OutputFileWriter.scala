package cz.fit.cvut
package Writers

import cats.effect.{Blocker, Concurrent, ContextShift, Fiber}
import cats.implicits._
import fs2.concurrent.Queue
import fs2.{io, text}

import java.nio.file.{Paths, StandardOpenOption}

trait OutputWriter[F[_]] {
  def write(item: String): F[Unit]

  def close: F[Unit]
}

object OutputFileWriter {
  def create[F[_]](
                    queue: Queue[F, Option[Either[Throwable, String]]],
                    destinationFile: String
                  )(implicit blocker: Blocker, F: Concurrent[F], cs: ContextShift[F]): F[(OutputWriter[F], Fiber[F, Unit])] = {

    def toFile(fileName: String)(implicit blocker: Blocker): fs2.Pipe[F, String, Unit] =
      _.intersperse("\n")
//        .evalTap { s =>
//          F.delay(println(s"[${Thread.currentThread().getName}] Writing $s to $fileName"))
//        }
        .through(text.utf8Encode)
        .through(io.file.writeAll(Paths.get(fileName), blocker, List(StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE, StandardOpenOption.CREATE)))

    def constantPoll: fs2.Stream[F, Unit] = {
      fs2.Stream.eval(F.delay(println(s"File $destinationFile writing queue started"))) *>
        queue.dequeue
          .unNoneTerminate
          .rethrow
          .through(toFile(destinationFile)(blocker))
          .onFinalize(F.delay(println(s"End of $destinationFile writing queue")))
    }

    for {
      // run deque routine asynchronously
      a <- F.start(fs2.Stream.bracket(constantPoll.compile.drain)(_ => queue.enqueue1(None)).compile.drain)
      writeToFileInstance <- F.delay {
        new OutputWriter[F] {
          override def write(item: String): F[Unit] = {
//            F.delay(println(s"[${Thread.currentThread().getName}] Enqueue new item: $item")) *>
              queue.enqueue1(Some(Right(item)))
          }

          override def close: F[Unit] = {
//            F.delay(println(s"[${Thread.currentThread().getName}] Closing")) *>
              queue.enqueue1(None)
          }
        }
      }
    } yield (writeToFileInstance, a)
  }
}