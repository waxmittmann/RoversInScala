package me.max.marscontrol.entity.rover

import me.max.marscontrol.entity.Orientation.{East, North, West}
import me.max.marscontrol.entity.{Move, TurnRight, TurnLeft, Position}
import org.mockito.Matchers.{eq => mockitoEq, _}
import org.specs2._
import specification.{Before, Scope}
import org.specs2.mock.Mockito

class CommandSpec extends mutable.Specification {
  "TurnLeft" should {
    "rotate a position left" in {
      val positionOrientation = RoverPositionOrientation(Position(1, 2), North)

      val newPositionOrientation = TurnLeft(positionOrientation)

      newPositionOrientation.orientation must beEqualTo(West)
      newPositionOrientation.position must beEqualTo(Position(1, 2))
    }
  }

  "TurnRight" should {
    "rotate a position right" in {
      val positionOrientation = RoverPositionOrientation(Position(1, 2), North)

      val newPositionOrientation = TurnRight(positionOrientation)

      newPositionOrientation.orientation must beEqualTo(East)
      newPositionOrientation.position must beEqualTo(Position(1, 2))
    }
  }

  "Move" should {
    "move forward" in {
      val positionOrientation = RoverPositionOrientation(Position(1, 2), North)

      val newPositionOrientation = Move(positionOrientation)

      newPositionOrientation.orientation must beEqualTo(North)
      newPositionOrientation.position must beEqualTo(Position(2, 2))
    }
  }
}