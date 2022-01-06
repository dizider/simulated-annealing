package cz.fit.cvut
package SimulatedAnnealing

import Operators.{Operators, SATOperator}

import java.io.File
import scala.annotation.tailrec
import scala.util.Random

trait SATLiteral extends Item {
  def number: Int

  def value: Boolean

  def isSatisfied: Boolean
}

case class RawSATLiteral(cost: Double, isPresented: Boolean) extends Item

case class SATSolverFactory()(implicit config: Config) {

  def withTimeBasedRandom: SATSolver = {
    SATSolver(TimeBasedRandomStrategy())
  }
}

class SATSolver(randomStrategyFactory: RandomStrategyFactory)(implicit config: Config) extends SimulatedAnnealing[SATState](Operators(List(SATOperator())), randomStrategyFactory)

object SATSolver {
  def apply(randomStrategyFactory: RandomStrategyFactory)(implicit config: Config): SATSolver = new SATSolver(randomStrategyFactory)
}

case class SAT(clauses: Array[SATClause])

final case class SATState(clauses: Array[SATClause], literalsValues: Map[Int, RawSATLiteral], randomStrategyFactory: RandomStrategyFactory)(implicit config: Config) extends State[SATState] {
  lazy val isSatisfiable: Boolean = cntSatisfiableClauses == clauses.length
  lazy val totalWeight: Double = literalsValues.values.foldLeft(0.0)((acc, l) => acc + l.cost)
  lazy val cntSatisfiableClauses: Int = clauses.count(_.isSatisfiable)
  lazy val percentOfSatisfiedClauses: Double = cntSatisfiableClauses / clauses.length.toDouble
  lazy val weight: Double = literalsValues.foldLeft(0.0)((acc, literal) => if (literal._2.isPresented) acc + literal._2.cost else acc)
  private lazy val random: CustomRandom = randomStrategyFactory.get()

  override lazy val isValid: Boolean = isSatisfiable

  def shuffle: SATState = {
    val shuffledLiterals: Map[Int, RawSATLiteral] = this.literalsValues.map(a => (a._1, RawSATLiteral(a._2.cost, random.nextBoolean)))
    val newClauses = this.clauses.map(clause => clause.updateLiterals(shuffledLiterals))
    val shuffled: SATState = SATState(newClauses, shuffledLiterals, randomStrategyFactory)
    //    if (shuffled.percentOfSatisfiedClauses > 0.95)
    shuffled
    //    else shuffle
  }

  override def valueOfOptimization(): Double = weight

  def cost(): Double = if (isSatisfiable) {
    weight // je reseni
  } else {
    // neni reseni -- relaxace, pokuta (procento nesplnenych frli)
    weight * percentOfSatisfiedClauses * config.correction
  }

  override def betterThan(that: SATState): Boolean = {
    this.cost() >= that.cost()
  }

  override def howMuchWorstThan(that: SATState): Double = {
    that.cost() - this.cost()
  }

  override def toString: String = s"$weight ${literalsValues.toList.sortWith((a, b) => a._1 < b._1).map(a => if (a._2.isPresented) a._1.toString else (-a._1).toString).mkString(" ")}"
}

case class SATPosLiteral(value: Boolean, weight: Double, number: Int) extends SATLiteral {
  override def toString: String = s"$number"

  override def cost: Double = weight

  override def isPresented: Boolean = isSatisfied

  override def isSatisfied: Boolean = value
}

case class SATNotLiteral(value: Boolean, weight: Double, number: Int) extends SATLiteral {
  override def toString: String = s"not $number"

  override def cost: Double = 0

  override def isPresented: Boolean = isSatisfied

  override def isSatisfied: Boolean = !value
}

case class SATClause(literals: List[SATLiteral]) {
  override def toString: String = s"$weight ${literals.mkString(" ")}"

  lazy val literalsNo: List[Int] = literals.map(l => l.number)

  private def weight: Double = if (isSatisfiable) literals.foldLeft(0.0)((acc, literal) => acc + literal.cost) else 0

  lazy val isSatisfiable: Boolean = literals.foldLeft(false)((acc, literal) => acc || literal.isSatisfied)

  def updateLiterals(literalsValues: Map[Int, RawSATLiteral]): SATClause = {
    // unsafe access to elements of literals values
    SATClause(literals.map {
      case SATNotLiteral(_, weight, number) => SATNotLiteral(literalsValues(number).isPresented, weight, number)
      case SATPosLiteral(_, weight, number) => SATPosLiteral(literalsValues(number).isPresented, weight, number)
    })
  }

}