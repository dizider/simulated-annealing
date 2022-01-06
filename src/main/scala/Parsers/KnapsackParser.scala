package cz.fit.cvut
package Parsers

import SimulatedAnnealing.{Knapsack, KnapsackItem}

import monix.eval.Task
import monix.reactive.Observable

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

class KnapsackParser(val source: Source) extends InputParser[Knapsack] {

  override def parse(): Observable[Knapsack] = {
    Observable.fromIterator(Task(source.getLines().map { line =>
      val parsedLine = line.split(" ")
      val partialKnapsack = Knapsack(parsedLine(0).toInt, parsedLine(1).toInt, parsedLine(2).toInt, _)
      partialKnapsack(parseItems(parsedLine.drop(3)))
    }))
  }

  @tailrec
  final def parseItems(rawItems: Array[String], acc: Queue[KnapsackItem] = Queue()): List[KnapsackItem] = {
    if (rawItems.length == 2) acc.toList
    else {
      parseItems(rawItems.drop(2), acc.appended(KnapsackItem(rawItems(1).toInt, rawItems(0).toInt, isPresented = false)))
    }
  }
}