package cz.fit.cvut
package Parsers

import SimulatedAnnealing.{Knapsack, KnapsackItem}

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source


class KnapsackParser(val source: Source) extends InputParser[Knapsack] {

  override def parse(): List[Knapsack] = {
    var knapsacks: Array[Knapsack] = Array()
    for (line <- source.getLines()) {
      val parsedLine = line.split(" ")
      val partialKnapsack = Knapsack(parsedLine(0).toInt, parsedLine(1).toInt, parsedLine(2).toInt, _)
      val items = parseItems(parsedLine.drop(3))
      knapsacks = knapsacks :+ partialKnapsack(items)
    }
    knapsacks.toList
  }

  @tailrec
  final def parseItems(rawItems: Array[String], acc: Queue[KnapsackItem] = Queue()): List[KnapsackItem] = {
    if (rawItems.length == 2) acc.toList
    else {
      parseItems(rawItems.drop(2), acc.appended(KnapsackItem(rawItems(1).toInt, rawItems(0).toInt, isPresented = false)))
    }
  }
}