package cz.fit.cvut
package Parsers

import SimulatedAnnealing.{Knapsack, KnapsackItem, KnapsackState}

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random


class KnapsackParser(val source: Source) extends InputParser[Knapsack] {

  private val random = {
    val r = Random
    Random.setSeed(System.currentTimeMillis())
    r
  }

  private def randomIsPresented: Boolean = {
    if (random.nextDouble() < 0.5) false
    else true
  }

  @tailrec
  private def randomState(state: KnapsackState): KnapsackState = {
    val random = KnapsackState(state.items.map(it => KnapsackItem(it.cost, it.weight, randomIsPresented)), state.knapsack)
    if (random.weight > state.knapsack.size) randomState(random)
    else random
  }

  override def parse(): List[Knapsack] = {
    var knapsacks: Array[Knapsack] = Array()
    for (line <- source.getLines()) {
      val parsedLine = line.split(" ")
      val partialKnapsack = Knapsack(parsedLine(0).toInt, parsedLine(1).toInt, parsedLine(2).toInt, _)
      var items: Array[KnapsackItem] = Array()
      for (i <- 3 until parsedLine.size by 2) {
        items = items :+ KnapsackItem(parsedLine(i + 1).toInt, parsedLine(i).toInt, randomIsPresented)
      }
      knapsacks = knapsacks :+ partialKnapsack(randomState(KnapsackState(items.toList, partialKnapsack(items.toList))).items)
    }
    knapsacks.toList
  }
}