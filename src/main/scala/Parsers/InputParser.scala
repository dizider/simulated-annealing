package cz.fit.cvut
package Parsers

import scala.io.Source

trait InputParser[+A] {
  val source: Source

  def parse(): List[A]
}
