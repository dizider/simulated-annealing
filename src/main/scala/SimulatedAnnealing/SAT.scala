package cz.fit.cvut
package SimulatedAnnealing

import Operators.{Operators, SATOperator}
import Writers.OutputWriter

import cats.effect.Concurrent

trait SATLiteral extends Item {
  def number: Int

  def value: Boolean

  def isSatisfied: Boolean

  def isSatisfied(value: Boolean): Boolean
}

case class RawSATLiteral(cost: Double, isPresented: Boolean) extends Item

case class SATSolverFactory[F[_]](randomStrategyFactory: RandomStrategyFactory = TimeBasedRandomStrategy(), writer: Option[OutputWriter[F]] = None)(implicit config: Config, F: Concurrent[F]) {
  def withOutputWriter(writer: OutputWriter[F]): SATSolverFactory[F] = {
    SATSolverFactory(randomStrategyFactory, Some(writer))
  }

  def build: SATSolver[F] = {
    new SATSolver[F](randomStrategyFactory, writer)
  }
}

class SATSolver[F[_]](randomStrategyFactory: RandomStrategyFactory, queue: Option[OutputWriter[F]])(implicit config: Config, F: Concurrent[F]) extends SimulatedAnnealing[F, SATState](Operators(List(SATOperator())), randomStrategyFactory, queue)(config, F)

case class SAT(clauses: Array[SATClause])

final case class SATState(clauses: Array[SATClause], literalsValues: Map[Int, RawSATLiteral], randomStrategyFactory: RandomStrategyFactory)(implicit config: Config) extends State[SATState] {
  lazy val cntSatisfiableClauses: Int = clauses.count(_.isSatisfiable(literalsValues))
  lazy val isSatisfiable: Boolean = cntSatisfiableClauses == clauses.length
  lazy val percentOfSatisfiedClauses: Double = cntSatisfiableClauses.toDouble / clauses.length.toDouble
  private lazy val weight: Double = literalsValues.foldLeft(0.0)((acc, literal) => if (literal._2.isPresented) acc + literal._2.cost else acc)
  private lazy val random: CustomRandom = randomStrategyFactory.get()

  override lazy val isValid: Boolean = isSatisfiable

  def shuffle: SATState = {
    val shuffledLiterals: Map[Int, RawSATLiteral] = this.literalsValues.map(a => (a._1, RawSATLiteral(a._2.cost, random.nextBoolean)))
    val shuffled: SATState = SATState(clauses, shuffledLiterals, randomStrategyFactory)
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
    math.abs(that.cost() - this.cost())
  }

  override def toString: String = s"$weight ${literalsValues.toList.sortWith((a, b) => a._1 < b._1).map(a => if (a._2.isPresented) a._1.toString else (-a._1).toString).mkString(" ")}"
}

case class SATPosLiteral(value: Boolean, weight: Double, number: Int) extends SATLiteral {
  override def toString: String = s"$number"

  override def cost: Double = weight

  override def isPresented: Boolean = isSatisfied

  override def isSatisfied: Boolean = value

  override def isSatisfied(value: Boolean): Boolean = if (value) true else false
}

case class SATNotLiteral(value: Boolean, weight: Double, number: Int) extends SATLiteral {
  override def toString: String = s"not $number"

  override def cost: Double = 0

  override def isPresented: Boolean = isSatisfied

  override def isSatisfied: Boolean = !value

  override def isSatisfied(value: Boolean): Boolean = if (!value) true else false
}

case class SATClause(literals: Array[SATLiteral]) {
  override def toString: String = s"${literals.mkString(" ")}"

  def isSatisfiable(values: Map[Int, RawSATLiteral]): Boolean = literals.foldLeft(false)((acc, literal) => acc || literal.isSatisfied(values(literal.number).isPresented))
}