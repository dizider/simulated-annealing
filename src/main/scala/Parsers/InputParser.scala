package cz.fit.cvut
package Parsers

import cats.effect.IO
import fs2.Pipe

trait InputParser[+A] {
  def parse(): Pipe[IO, String, A]
}
