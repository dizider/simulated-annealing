package cz.fit.cvut

import java.io.File
import scopt.{OParser, OParserBuilder}

case class Config(
                   input: File = new File("input.txt"),
                   timeOutput: File = new File("time.txt"),
                   output: File = new File("output.txt"),
                   partialSolutionOutput: Option[File] = None,
                   innerLoop: Option[Int] = None,
                   relaxationCoefficient: Double = 0.5,
                   correction: Double = 0.85,
                   initTemperature: Double = 50,
                   coolingCoefficient: Double = 0.995,
                   frozenTemperature: Double = 0.5)

object ArgParser {
  val builder: OParserBuilder[Config] = OParser.builder[Config]
  val parser1: OParser[Unit, Config] = {
    import builder._
    OParser.sequence(
      programName("simulated-annealing"),
      head("Simulated annealing solver", "1.0"),
      opt[Int]('e', "equilibrium")
        .action((x, c) => c.copy(innerLoop = Some(x)))
        .text("equilibrium is an integer property"),
      opt[Double]('t', "init_temperature")
        .action((x, c) => c.copy(initTemperature = x))
        .text("init temperature is an integer property"),
      opt[Double]('c', "cool")
        .action((x, c) => c.copy(coolingCoefficient = x))
        .text("cooling coefficient is an integer property"),
      opt[Double]('f', "frozen")
        .action((x, c) => c.copy(frozenTemperature = x))
        .text("frozen temperature is an integer property"),
      opt[Double]('n', "correction")
        .action((x, c) => c.copy(correction = x))
        .text("frozen temperature is an integer property"),
      opt[Double]('r', "relaxation")
        .action((x, c) => c.copy(relaxationCoefficient = x))
        .text("frozen temperature is an integer property"),
      opt[File]('o', "out")
        .required()
        .valueName("<file>")
        .action((x, c) => c.copy(output = x))
        .text("output file is a required file property"),
      opt[File]('i', "input")
        .required()
        .valueName("<file>")
        .action((x, c) => c.copy(input = x))
        .text("input file is a required file property"),
      opt[File]('m', "time")
        .required()
        .valueName("<file>")
        .action((x, c) => c.copy(timeOutput = x))
        .text("output time file is a required file property"),
      opt[File]('p', "partial")
        .valueName("<file>")
        .action((x, c) => c.copy(partialSolutionOutput = Some(x)))
        .text("output file for partial solutions")
    )
  }
}