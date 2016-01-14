package me.max.marscontrol.main

import me.max.marscontrol.entity._
import me.max.marscontrol.entity.rover.{Rovers, RoversAccumulator, RoverPositionOrientation}
import Orientation.{East, North}

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
