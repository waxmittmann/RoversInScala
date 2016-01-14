package me.max.marscontrol.main

import me.max.marscontrol.entity.rover.{RoverError, Rovers, RoversAccumulator}
import me.max.marscontrol.util.CommandParser

object Main_FromStdIn {
  def main(args: Array[String]): Unit = {
    val input =
      "5 5\n" +
      "1 2 N\n" +
      "LMLMLMLMM\n" +
      "3 3 E\n" +
      "MMRMMRMRRM\n"

    val result = (for {
      roversInput <- CommandParser.parse(input).left.map(err => s"Failed with $err").right
      result <- {
        val initialRovers: Rovers = Rovers(roversInput._1, roversInput._2)
        val finalState = RoversAccumulator(initialRovers).executeAll(roversInput._3)

        val result = finalState.state.fold({
          case (error: RoverError, states: List[Rovers]) => {
            val previousStates = states.mkString("\n")
            Left(s"Commands failed with error ${error}.\nStates leading up to the error:\n ${previousStates}")
          }
        }, {
          case (finalState: Rovers, states: List[Rovers]) => {
            Right(finalState.toString())
          }
        })
        result
      }.right
    } yield result).merge

    println("Final result is: " + result)
  }
}
