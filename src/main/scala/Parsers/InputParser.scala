package cz.fit.cvut
package Parsers

import monix.reactive.Observable

import scala.io.Source

trait InputParser[+A] {
  val source: Source

  def parse(): Observable[A]
}
