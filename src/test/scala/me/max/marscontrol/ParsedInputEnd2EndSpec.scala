package me.max.marscontrol

import me.max.marscontrol.entity.Orientation.{West, East, North}
import me.max.marscontrol.entity.rover.{RoversAccumulator, Rovers, RoverPositionOrientation}
import me.max.marscontrol.parser.CommandParser
import org.specs2.matcher.{MatchResult, BeEqualTo}
import org.specs2.mutable.Specification

class ParsedInputEnd2EndSpec extends Specification {
  def checkState(state: Rovers, expected: List[RoverPositionOrientation]): MatchResult[RoverPositionOrientation] = {
    val matcherResults = state.rovers.zip(expected).map(cur => (cur._1 must beEqualTo(cur._2)))
    matcherResults.find(!_.isSuccess).getOrElse(matcherResults(0))
  }

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
      parseResult.isRight must beTrue
      parseResult.right.get.state.isRight must beTrue

      val states: (Rovers, List[Rovers]) = parseResult.right.get.state.right.get
      val history = states._2
      history.length must beEqualTo(2) //Should be 1?

      val firstState = history(0)
      val secondState = history(1)
      val finalState = states._1

      checkState(firstState, List(RoverPositionOrientation(1, 2, West), RoverPositionOrientation(4, 3, East)))
      checkState(secondState, List(RoverPositionOrientation(0, 2, West), RoverPositionOrientation(5, 3, East)))
      //Currently same as secondState
      checkState(finalState, List(RoverPositionOrientation(0, 2, West), RoverPositionOrientation(5, 3, East)))
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
      parseResult.isRight must beTrue
      parseResult.right.get.state.isRight must beTrue

      val states: (Rovers, List[Rovers]) = parseResult.right.get.state.right.get

      val finalState = states._1
      checkState(finalState, List(RoverPositionOrientation(1, 3, North), RoverPositionOrientation(5, 1, East)))
    }
  }
}

