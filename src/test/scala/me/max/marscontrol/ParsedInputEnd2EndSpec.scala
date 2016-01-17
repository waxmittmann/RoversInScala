package me.max.marscontrol

import me.max.marscontrol.entity.Orientation.{West, East, North}
import me.max.marscontrol.entity.Position
import me.max.marscontrol.entity.rover.{RoverError, RoversAccumulator, Rovers, RoverPositionOrientation}
import me.max.marscontrol.parser.CommandParser
import org.specs2.mutable.Specification
import org.specs2.specification.Scope

/*
    1 2 N, 3 3 E
    1 2 W,
 */

class ParsedInputEnd2EndSpec extends Specification {
  "When running rovers using RoversAccumulator and input parsed from a string" should {
    "correctly handle a simple example" in {
      //Given
      val input =
        "5 5\n" +
        "1 2 N\n" +
        "LM\n" +
        "3 3 E\n" +
        "MM\n"

      //When
      val parseResult: Either[String, RoversAccumulator] = (for {
        roversInput <- CommandParser.parse(input).left.map(err => s"Failed with $err").right
      } yield RoversAccumulator(roversInput))

      //Then
      println(parseResult)

      parseResult.isRight must beTrue
      parseResult.right.get.state.isRight must beTrue

      val states: (Rovers, List[Rovers]) = parseResult.right.get.state.right.get
      val history = states._2
      history.length must beEqualTo(2) //Should be 1?

      val firstState = history(0)
      val secondState = history(1)
      val finalState = states._1
      println(s"First state:\n$firstState\nSecond state:\n$secondState")

      firstState.rovers(0) must beEqualTo(RoverPositionOrientation(1, 2, West))
      firstState.rovers(1) must beEqualTo(RoverPositionOrientation(4, 3, East))

      secondState.rovers(0) must beEqualTo(RoverPositionOrientation(0, 2, West))
      secondState.rovers(1) must beEqualTo(RoverPositionOrientation(5, 3, East))

      //Currently same as secondState
      finalState.rovers(0) must beEqualTo(RoverPositionOrientation(0, 2, West))
      finalState.rovers(1) must beEqualTo(RoverPositionOrientation(5, 3, East))
    }

    "initial input produce the correct result" in {
      //Given
      val input =
        "5 5\n" +
          "1 2 N\n" +
          "LMLMLMLMM\n" +
          "3 3 E\n" +
          "MMRMMRMRRM\n"

      //When
      val parseResult: Either[String, RoversAccumulator] = (for {
        roversInput <- CommandParser.parse(input).left.map(err => s"Failed with $err").right
      } yield RoversAccumulator(roversInput))

      //Then
      println(parseResult)

      parseResult.isRight must beTrue
      parseResult.right.get.state.isRight must beTrue

      val states: (Rovers, List[Rovers]) = parseResult.right.get.state.right.get

      val finalState = states._1
      finalState.rovers(0) must beEqualTo(RoverPositionOrientation(1, 3, North))
      finalState.rovers(1) must beEqualTo(RoverPositionOrientation(5, 1, East))
    }
  }
}

