package me.max.marscontrol.main

import me.max.marscontrol.entity.rover.{RoverError, Rovers, RoversAccumulator}
import me.max.marscontrol.parser.CommandParser

object Main_FromInputString {
  def main(args: Array[String]): Unit = {
    def transformToResult(finalState: RoversAccumulator) = finalState.state.fold({
      case (error: RoverError, states: List[Rovers]) =>
        val previousStates = states.mkString("\n")
        Left(s"Commands failed with error ${error}.\nStates leading up to the error:\n ${previousStates}")
    }, {
      case (finalState: Rovers, states: List[Rovers]) =>
        Right(finalState.toString())
    })

    val input =
      "5 5\n" +
      "1 2 N\n" +
      "LMLMLMLMM\n" +
      "3 3 E\n" +
      "MMRMMRMRRM\n"

    val result = (for {
      roversInput <- CommandParser.parse(input).left.map(err => s"Failed with $err").right
      result <- {
        val initialRovers: Rovers = Rovers(roversInput.plateauDimensions, roversInput.rovers)
        val finalState = RoversAccumulator(initialRovers).executeAll(roversInput.commands)
        transformToResult(finalState)
      }.right
    } yield result).merge

    println("Final result is: " + result)
  }
}
