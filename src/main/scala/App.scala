package cz.fit.cvut

import Parsers.SATParser
import SimulatedAnnealing._
import Writers.SATWriter

import cats.effect.{ExitCode, Resource}
import monix.eval.{Task, TaskApp}
import monix.reactive.OverflowStrategy
import scopt.OParser

import java.io.FileWriter
import scala.io.Source
import scala.language.implicitConversions


object App extends TaskApp {

  def solveKnapsackInstance(knapsack: Knapsack)(implicit config: Config): KnapsackState = {
    val initState: KnapsackState = KnapsackState(knapsack.items, knapsack, TimeBasedRandomStrategy()).shuffle
    KnapsackSolverFactory()
      .withTimeBasedRandom
      .solve(Temperature(config.initTemperature), initState, initState)(t => t.value <= config.frozenTemperature, config.innerLoop.getOrElse(knapsack.noItems).toDouble, c => c * config.coolingCoefficient)
  }

  def solveSATInstance(sat: SATState)(implicit config: Config): SATState = {
    val initState = sat.shuffle
    SATSolverFactory()
      .withTimeBasedRandom
      .solve(Temperature(config.initTemperature), initState, initState)(t => t.value <= config.frozenTemperature, config.innerLoop.getOrElse(sat.literalsValues.size * 2).toDouble, c => c * config.coolingCoefficient)
  }

  def fileReader(name: String): Resource[Task, Source] = Resource.make(Task(Source.fromFile(name)))(f => Task(f.close()))

  def fileWriter(name: String): Resource[Task, FileWriter] = Resource.make(Task(new FileWriter(name)))(f => Task(f.close()))

  override def run(args: List[String]): Task[ExitCode] = {
    OParser.parse(ArgParser.parser1, args, Config()) match {
      case Some(config) =>
        (for {
          input <- fileReader(config.input.getPath)
          output <- fileWriter(config.output.getPath)
          timeOutput <- fileWriter(config.timeOutput.getPath)
        } yield (input, output, timeOutput)).use {
          case (input, output, time) =>
            new SATParser(input)(config)
              .parse()
              .map(sat => Timer.measureCpuTime(solveSATInstance(sat)(config)))
              .map(res => { // TODO rewrite to logger Pipe
                time.write(s"${res._2}\n");
                println(res._1)
                res
              })
              .mapEval(res => new SATWriter(output).println(res._1))
              .completedL
              .map(_ => ExitCode.Success)
          //            new KnapsackParser(input)
          //              .parse().map(knapsack => Timer.measureCpuTime(solveKnapsackInstance(knapsack, config)))
          //              .map(res => { // TODO rewrite to logger Pipe
          //                time.write(s"${res._2}\n");
          //                res
          //              })
          //              .asyncBoundary(OverflowStrategy.Unbounded)
          //              .mapEval(res => new KnapsackWriter(output).println(res._1))
          //              .completedL
          //              .map(_ => ExitCode.Success)
        }
      case _ =>
        println("Arguments are not valid :-(")
        Task(ExitCode.Error)
    }
  }
}
