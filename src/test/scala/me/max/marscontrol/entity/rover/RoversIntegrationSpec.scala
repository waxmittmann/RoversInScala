package me.max.marscontrol.entity.rover

import org.specs2.mutable.Specification
import me.max.marscontrol.entity.Orientation.{South, East, North, West}
import me.max.marscontrol.entity.{Move, TurnRight, TurnLeft, Position}
import org.mockito.Matchers.{eq => mockitoEq, _}
import org.specs2._
import org.specs2.specification.{Fixture, Before, Scope}
import org.specs2.mock.Mockito

class RoversIntegrationSpec extends Specification {
  trait TestData extends Scope {
    val plateau = (5, 5)
    val roverA = RoverPositionOrientation(Position(1, 0), North)
    val roverB = RoverPositionOrientation(Position(3, 5), East)
    val rovers = Rovers(plateau, List(roverA, roverB))
  }

  "Rovers.execute(...)" should {
    "execute commands correctly if the command list shape is correct" in {
      new TestData {
        val result = rovers.execute(List(Move, TurnLeft))

        result.isRight must beTrue
        val roverData = result.right.get.rovers
        roverData(0) must beEqualTo(RoverPositionOrientation(Position(1, 1), North))
        roverData(1) must beEqualTo(RoverPositionOrientation(Position(3, 5), North))
      }
    }

    "fail if there are too few commands for the rovers" in {
      new TestData {
        val result = rovers.execute(List(Move))

        result.isLeft must beTrue
        result.left.get must beEqualTo(RoverAndCommandShapesDontMatch)
      }
    }

    "fail if there are too many commands for the rovers" in {
      new TestData {
        val result = rovers.execute(List(Move, Move, Move))

        result.isLeft must beTrue
        result.left.get must beEqualTo(RoverAndCommandShapesDontMatch)
      }
    }
  }
}
