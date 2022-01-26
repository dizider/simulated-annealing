package cz.fit.cvut

import Parsers.SATParser
import SimulatedAnnealing._
import Writers.{OutputFileWriter, OutputWriter}

import cats.effect.{Blocker, Concurrent, ConcurrentEffect, ExitCode, IO, IOApp}
import fs2.concurrent.Queue
import scopt.OParser

import scala.language.implicitConversions

object App extends IOApp {

  //  def solveKnapsackInstance(knapsack: Knapsack)(implicit config: Config): KnapsackState = {
  //    val initState: KnapsackState = KnapsackState(knapsack.items, knapsack, TimeBasedRandomStrategy()).shuffle
  //    KnapsackSolverFactory()
  //      .withTimeBasedRandom
  //      .solve(Temperature(config.initTemperature), initState, initState)(t => t.value <= config.frozenTemperature, config.innerLoop.getOrElse(knapsack.noItems).toDouble, c => c * config.coolingCoefficient)
  //  }

  def solveSATInstance(src: SATState, fileWriter: OutputWriter[IO])(implicit config: Config): IO[SATState] = {
    for {
      _ <- IO.delay(println("Start solving SAT"))
      initState <- IO.delay(src.shuffle)
      a <- SATSolverFactory[IO]()
        .withOutputWriter(fileWriter)
        .build
        .solve(Temperature(config.initTemperature), initState, initState)(t => t.value <= config.frozenTemperature, config.innerLoop.getOrElse(src.literalsValues.size * 2).toDouble, c => c * config.coolingCoefficient)
    } yield a
  }

  override def run(args: List[String]): IO[ExitCode] = {
    OParser.parse(ArgParser.parser1, args, Config()) match {
      case Some(conf) =>
        implicit val config: Config = conf
        (for {
          q <- fs2.Stream.eval(Queue.bounded[IO, Option[Either[Throwable, String]]](10))
          qq <- fs2.Stream.eval(Queue.bounded[IO, Option[Either[Throwable, String]]](10))
          blocker <- fs2.Stream.resource(Blocker[IO])
          output <- fs2.Stream.eval(OutputFileWriter.create[IO](q, conf.output.getPath)(blocker, ConcurrentEffect[IO], contextShift))
          partialSolutionOutput <- fs2.Stream.eval(OutputFileWriter.create[IO](qq, conf.partialSolutionOutput.get.getPath)(blocker, ConcurrentEffect[IO], contextShift)) // TODO fix handling option file path
          effect <- InputFileReader.create(conf.input.getPath)(blocker, Concurrent[IO], contextShift)
            .through(SATParser().parse())
            .onFinalize(output._1.close)
            .onFinalize(partialSolutionOutput._1.close)
            .mapAsync(2)(src => solveSATInstance(src, partialSolutionOutput._1))
            .map(_.toString)
            .evalTap { s =>
              IO.delay(println(s"Problem solved: ${s}"))
            }
            .evalTap(s => output._1.write(s))
            .onFinalize(IO.delay(println("bye")))
        } yield effect).compile.drain.as(ExitCode.Success)
      case _ =>
        println("Arguments are not valid :-(")
        IO(ExitCode.Error)
    }
  }
}
