package cz.fit.cvut

import Parsers.KnapsackParser
import SimulatedAnnealing._
import Writers.KnapsackWriterBuilder

import scopt.OParser

import scala.io.Source
import scala.language.implicitConversions

object App {

  implicit val equilibrium: Double => Boolean = (number: Double) => {
    number > 50
  }

  implicit val cool: Temperature => Temperature = (temperature: Temperature) => {
    temperature * 0.995
  }

  implicit val frozen: Temperature => Boolean = (temperature: Temperature) => {
    temperature.value < 7.8
  }

  def main(args: Array[String]): Unit = {

    // OParser.parse returns Option[Config]
    OParser.parse(ArgParser.parser1, args, Config()) match {
      case Some(config) =>
        val knapsacks = new KnapsackParser(Source.fromFile(config.input)).parse()
        val writer = new KnapsackWriterBuilder().writeTo(config.output)
        val timeWriter = config.timeOutput.map(timeFile => new KnapsackWriterBuilder().writeTo(timeFile))

        knapsacks.foreach { knapsack =>
          val fw = writer.build.getOrElse(throw new RuntimeException("Output file error"))
          val tw = timeWriter.map(_.build.getOrElse(throw new RuntimeException("Time output file error")))
          val initState: KnapsackState = KnapsackState(knapsack.items, knapsack, TimeBasedRandomStrategy()).shuffle
          val solution = Timer.measureCpuTime(KnapsackSolverFactory(config).withTimeBasedRandom.solve(Temperature(config.initTemperature), initState, initState)(t => t.value < config.frozenTemperature, e => e > config.innerLoop, c => c * config.coolingCoefficient))
          fw.println(s"${solution._1.knapsack.id} ${solution._1.knapsack.noItems} ${solution._1.cost} ${solution._1}")
          tw.map(_.println(s"${solution._1.knapsack.id} ${solution._2}"))
        }
      case _ =>
        println("Arguments are not valid :-(")
    }
  }
}
