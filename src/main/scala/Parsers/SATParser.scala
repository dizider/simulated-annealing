package cz.fit.cvut
package Parsers

import SimulatedAnnealing.{RawSATLiteral, SATClause, SATNotLiteral, SATPosLiteral, SATState}

import monix.eval.Task
import monix.reactive.Observable

import scala.annotation.tailrec
import scala.io.Source

class SATParser(val source: Source)(implicit config: Config) extends InputParser[SATState] {

  override def parse(): Observable[SATState] = {
    Observable.fromTask {
      Task(source.getLines().removeComments().readWeights().readClauses().build())
    }
  }


  implicit class Operations(val iterator: Iterator[String]) {
    @tailrec
    final def readWeights(): SATInstanceBuilder = {
      val headerString = iterator.next()
      if (!headerString.startsWith("w")) readWeights()
      else {
        val weights: Array[Double] = headerString.replaceAll("([^0-9])", " ").trim.replaceAll(" +", " ").split(" ").map(w => w.toDouble).dropRight(1)
        val literals: Map[Int, RawSATLiteral] = (1 to weights.length).foldLeft(Map.empty[Int, RawSATLiteral])((acc, id) => acc + (id -> RawSATLiteral(weights(id - 1), isPresented = false)))
        SATInstanceBuilder(weights = weights, literals = literals, input = iterator)
      }
    }

    def removeComments(): Iterator[String] = iterator.filterNot(line => line.isEmpty || (line.head == 'c'))
  }

  implicit class SATInstanceBuilderOperations(val builder: SATInstanceBuilder) {
    def readClauses(): SATInstanceBuilder = {
      builder.input.foldLeft(builder) { (acc, clauseString) =>
        acc.addClause(SATClause(clauseString.trim.split(" ").dropRight(1).map(clause => (clause.toInt compareTo 0) match {
          case -1 => SATNotLiteral(value = false, builder.weights((-clause.toInt) - 1), -clause.toInt)
          case 1 => SATPosLiteral(value = false, builder.weights(clause.toInt - 1), clause.toInt)
        }).toList))
      }
    }
  }
}

sealed case class SATInstanceBuilder(clauses: Array[SATClause] = Array(), weights: Array[Double] = Array(), literals: Map[Int, RawSATLiteral], input: Iterator[String])(implicit config: Config) {
  def withWeights(w: Array[Double]): SATInstanceBuilder = SATInstanceBuilder(clauses, w, literals, input)

  def addClause(clause: SATClause): SATInstanceBuilder =
    if (clause.literals.size == 3)
      SATInstanceBuilder(clauses.appended(clause), weights, literals, input)
    else
      this

  def build(): SATState = SATState(clauses, literals, TimeBasedRandomStrategy())
}
