package cz.fit.cvut

import java.lang.management.{ManagementFactory, ThreadMXBean}
import scala.util.Random

object Timer {
  val bean: ThreadMXBean = ManagementFactory.getThreadMXBean

  def getCpuTime: Long = if (bean.isCurrentThreadCpuTimeSupported) bean.getCurrentThreadCpuTime else 0

  def measureCpuTime[T](f: => T): (T, Double) = {
    val start = getCpuTime
    val r = f
    val end = getCpuTime
    val t = (end - start) / 1000000000.0
    (r, t)
  }
}

trait RandomStrategyFactory {
  def get(): CustomRandom
}

trait CustomRandom {
  def nextDouble: Double

  def nextBoolean: Boolean

  def intInRange(from: Int, to: Int): Int
}

case class TimeBasedRandomStrategy() extends RandomStrategyFactory {
  override def get(): CustomRandom = TimeBasedRandom
}

object TimeBasedRandom extends CustomRandom {
  private val random: Random = {
    val r = new java.util.Random()
    r.setSeed(System.currentTimeMillis())
    r
  }

  override def nextDouble: Double = random.nextDouble()

  override def intInRange(from: Int, to: Int): Int = random.between(from, to)

  override def nextBoolean: Boolean = random.nextBoolean()
}

//trait Shuffle[A] {
//  def shuffle(a: A): A
//}

//object ShuffleSyntax {
//  implicit class ShuffleOps[A](value: A) {
//    def speak(implicit s: Shuffle[A]): A = {
//      s.shuffle(value)
//    }
//  }
//}
//
//
//object ShuffleOperations {
//
//  //  implicit val KnapsackShuffle = new Shuffle[KnapsackState] {
//  //
//  //  }
//
//  implicit val SATShuffle: Shuffle[SATState] = new Shuffle[SATState] {
//    @tailrec
//    override def shuffle(state: SATState): SATState = {
//      val shuffledLiterals: Array[SATLiteral] = state.literals.map {
//        case SATNotLiteral(_, weight, number) => SATNotLiteral(Random.nextFloat() < 0.5, weight, number)
//        case SATPosLiteral(_, weight, number) => SATPosLiteral(Random.nextFloat() < 0.5, weight, number)
//      }
//      val random: SATState = SATState(state.clauses, shuffledLiterals)
//      if (!random.isSatisfiable) shuffle(state)
//      else random
//    }
//  }
//}