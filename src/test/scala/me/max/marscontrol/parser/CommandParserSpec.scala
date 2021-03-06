package me.max.marscontrol.parser

import me.max.marscontrol.entity.Orientation.{West, East, North}
import me.max.marscontrol.entity._
import me.max.marscontrol.entity.rover.{RoverPositionOrientation, RoversInput}
import me.max.marscontrol.parser.CommandParser
import org.specs2.mutable.Specification

class CommandParserSpec extends Specification {

  "CommandParser" should {
    "parse input consisting only of the plateau dimensions" in {
      val input = "123 456"

      val result: Either[String, RoversInput] = CommandParser.parse(input)

      result.isRight must beTrue
      val output = result.right.get
      output.rovers.plateau._1 must beEqualTo(123)
      output.rovers.plateau._2 must beEqualTo(456)
      output.rovers.rovers.isEmpty must beTrue
      output.commands.isEmpty must beTrue
    }

    "fail on empty input" in {
      val input = ""

      val result: Either[String, RoversInput] = CommandParser.parse(input)

      result.isLeft must beTrue
    }

    "fail if plateau dimension is NaN" in {
      val input = "5 f"

      val result: Either[String, RoversInput] = CommandParser.parse(input)

      result.isLeft must beTrue
    }

    "fail if a rover is provided without a line for commands" in {
      val input =
        s"""123 456
           |5 8 E
         """.stripMargin

      val result: Either[String, RoversInput] = CommandParser.parse(input)

      result.isLeft must beTrue
    }

    //Doesn't work because .split("\n") strips empty lines; probably some kind of mode to turn that off?
    "succeed if a rover and an empty line of commands are provided" in {
      val input =
        s"""123 456
           |5 8 E
           |
           |""".stripMargin

      val result: Either[String, RoversInput] = CommandParser.parse(input)

      result.isRight must beTrue
    }.pendingUntilFixed

    "succeed if a rover and commands are provided" in {
      val input =
        s"""123 456
           |5 8 E
           |RLLMRLL""".stripMargin

      val result: Either[String, RoversInput] = CommandParser.parse(input)

      result.isRight must beTrue
    }

    "correctly parse two rover / command pairs" in {
      //Given
      val input =
        s"""123 456
           |5 8 E
           |RLLMRLL
           |3 1 W
           |MM""".stripMargin

      //When
      val result: Either[String, RoversInput] = CommandParser.parse(input)

      //Then
      result.isRight must beTrue
      val roversInput = result.right.get

      roversInput.rovers.rovers(0) must beEqualTo(RoverPositionOrientation(5, 8, East))
      roversInput.rovers.rovers(1) must beEqualTo(RoverPositionOrientation(3, 1, West))

      roversInput.commands(0) must beEqualTo(List(TurnRight, TurnLeft, TurnLeft, Move, TurnRight, TurnLeft, TurnLeft))
      roversInput.commands(1) must beEqualTo(List(Move, Move, Noop, Noop, Noop, Noop, Noop))
    }
  }

  "RoverCommand" should {
    "correctly parse an empty line of commands" in {
      val roverCommands = RoverCommands(List(""), (1, 1), RoverPositionOrientation(Position(1, 1), North), List())

      val result = roverCommands.process()

      result.isRight must beTrue
    }

    "correctly parse a line of commands" in {
      val roverCommands = RoverCommands(List("LMRMRL"), (1, 1), RoverPositionOrientation(Position(1, 1), North), List())

      val result = roverCommands.process()

      result.isRight must beTrue
      result.right.get match {
        case RoverDefinitionOrEnd(_, _, li) => li.head._2 must beEqualTo(List(TurnLeft, Move, TurnRight, Move, TurnRight, TurnLeft))
        case wrongState => failure(s"Incorrect state: $wrongState")
      }
      true
    }

  }


}
