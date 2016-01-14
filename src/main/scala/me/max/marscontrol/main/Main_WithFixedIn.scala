package me.max.marscontrol.main

import me.max.marscontrol.entity._
import me.max.marscontrol.entity.rover.{Rovers, RoversAccumulator, RoverPositionOrientation}
import Orientation.{East, North}

object Main_WithFixedIn {
  def main(args: Array[String]): Unit = {
    val initialRovers = Rovers((5, 5), List())
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
