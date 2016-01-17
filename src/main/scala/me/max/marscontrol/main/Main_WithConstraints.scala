package me.max.marscontrol.main

import me.max.marscontrol.entity.Orientation.{South, East, North}
import me.max.marscontrol.entity._
import me.max.marscontrol.entity.rover._

object Main_WithConstraints {
  def main(args: Array[String]): Unit = {
    val initialRovers = Rovers((5, 5), List())
      .addConstraint(Constraints.StayWithinPlateau)
      .addRover(RoverPositionOrientation(Position(0, 0), South))
      .addRover(RoverPositionOrientation(Position(0, 1), South))

    {
      val finalState: RoversAccumulator = RoversAccumulator(initialRovers)
        .execute(List(Move, Noop))

      println("Final state:\n" + finalState.toString)
    }
  }
}
