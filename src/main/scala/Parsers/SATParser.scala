package cz.fit.cvut
package Parsers

import SimulatedAnnealing._

import cats.effect.IO
import fs2.Pipe

class SATParser()(implicit config: Config) extends InputParser[SATState] {
  def parse(): Pipe[IO, String, SATState] = src =>
    src
      .through(removeComments)
      .through(readSATObjects)
      .fold(SATInstanceBuilder())((acc, builder) => acc + builder)
      .map(_.build())

  val removeComments: Pipe[IO, String, String] = src =>
    src.filter(_.nonEmpty).filter(!_.startsWith("c")).filter(!_.startsWith("p"))

  val readSATObjects: Pipe[IO, String, SATInstanceBuilder] = src => {
    src.map { head =>
      if (head.startsWith("w")) {
        readWeights(head)
      } else {
        readClause(head)
      }
    }
  }

  private val readWeights: String => SATInstanceBuilder = inputString => {
    val weights: Map[Int, Double] =
      inputString
        .replaceAll("([^0-9])", " ")
        .trim
        .replaceAll(" +", " ")
        .split(" ")
        .map(w => w.toDouble)
        .dropRight(1)
        .zipWithIndex
        .map(_.swap)
        .map(a => a._1 + 1 -> a._2)
        .toMap
    val literals: Map[Int, RawSATLiteral] =
      (1 to weights.size)
        .foldLeft(Map.empty[Int, RawSATLiteral])((acc, id) => acc + (id -> RawSATLiteral(weights(id), isPresented = false)))
    SATInstanceBuilder(weights = weights, literals = literals)
  }

  val readClause: String => SATInstanceBuilder = inputString => {
    SATInstanceBuilder(clauses = Array(SATClause(inputString.trim.split(" ").dropRight(1).map(clause => (clause.toInt compareTo 0) match {
      case -1 => SATNotLiteral(value = false, 0, -clause.toInt)
      case 1 => SATPosLiteral(value = false, 0, clause.toInt)
    }))))
  }
}

object SATParser {
  def apply()(implicit config: Config) = new SATParser()
}

sealed case class SATInstanceBuilder(clauses: Array[SimulatedAnnealing.SATClause] = Array(), weights: Map[Int, Double] = Map.empty, literals: Map[Int, RawSATLiteral] = Map.empty)(implicit config: Config) {
  def withWeights(w: Map[Int, Double]): SATInstanceBuilder = SATInstanceBuilder(clauses, w, literals)

  def withLiterals(l: Map[Int, RawSATLiteral]): SATInstanceBuilder = SATInstanceBuilder(clauses, weights, l)

  def addClause(clause: SimulatedAnnealing.SATClause): SATInstanceBuilder =
    if (clause.literals.length == 3)
      SATInstanceBuilder(clauses.appended(clause), weights, literals)
    else
      this

  def build(): SATState = {
    println("Building instance")
    SATState(clauses, literals, TimeBasedRandomStrategy())
  }

  def +(that: SATInstanceBuilder): SATInstanceBuilder = {
    SATInstanceBuilder(this.clauses ++ that.clauses, this.weights ++ that.weights, this.literals ++ that.literals)
  }
}