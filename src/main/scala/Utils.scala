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
  private val random: Random = Random

  override def nextDouble: Double = random.nextDouble()

  override def intInRange(from: Int, to: Int): Int = random.between(from, to)

  override def nextBoolean: Boolean = random.nextBoolean()
}