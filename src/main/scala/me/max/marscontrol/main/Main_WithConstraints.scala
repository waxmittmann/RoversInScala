package me.max.marscontrol.main

import me.max.marscontrol.entity.Orientation.{South, East, North}
import me.max.marscontrol.entity._
import me.max.marscontrol.entity.rover.{StayWithinPlateau, RoverPositionOrientation, Rovers, RoversAccumulator}

object Main_WithConstraints {
  def main(args: Array[String]): Unit = {
    val initialRovers = Rovers((5, 5), List())
      .addConstraint(StayWithinPlateau)
      .addRover(RoverPositionOrientation(Position(0, 0), South))
      .addRover(RoverPositionOrientation(Position(0, 1), South))

    {
      val finalState = RoversAccumulator(initialRovers)
        .execute(List(Move, Noop))
//        .execute(List(Move, Move))

      println("Final state:\n" + finalState.toString)
    }

//    {
//      val finalState = RoversAccumulator(initialRovers)
//        .execute(List(Noop, Move))
//        .state
//
//      println(finalState)
//    }

  }
}
