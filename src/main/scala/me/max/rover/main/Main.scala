package me.max.rover.main

import me.max.rover.entity._
import Orientation.{East, North}
import me.max.rover._

object Main {
  def main(args: Array[String]): Unit = {
    val input =
      """
        |5 5
        |1 2 N
        |LMLMLMLMM
        |3 3 E
        |MMRMMRMRRM
      """.stripMargin

      val initialRovers = Rovers.empty((5, 5))
        .add(RoverPositionOrientation(Position(1, 1), North))
        .add(RoverPositionOrientation(Position(1, 2), East))

      val finalState = RoversAccumulator(initialRovers)
          .execute(List(Move, TurnLeft))
          .execute(List(Move, TurnLeft))
          .execute(List(Move, TurnLeft))
          .execute(List(Move, TurnLeft))
          .state

    println(finalState)
  }
}
