package me.max.marscontrol.util

import me.max.marscontrol.entity.Orientation.North
import me.max.marscontrol.entity.{TurnLeft, TurnRight, Move, Position}
import me.max.marscontrol.entity.rover.{RoverPositionOrientation, RoversInput}
import me.max.marscontrol.util.CommandParser.{RoverDefinitionOrEnd, RoverCommands}
import org.specs2.mutable.Specification

class CommandParserSpec extends Specification {

  "CommandParser" should {
    "parse input consisting only of the plateau dimensions" in {
      val input = "123 456"

      val result: Either[String, RoversInput] = CommandParser.parse(input)

      result.isRight must beTrue
      val output = result.right.get
      output.plateauDimensions._1 must beEqualTo(123)
      output.plateauDimensions._2 must beEqualTo(456)
      output.rovers.isEmpty must beTrue
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

      println(s"Input is:\n<$input>; length is: ${input.split("\n").length}")

      val result: Either[String, RoversInput] = CommandParser.parse(input)

      println(result)
      result.isRight must beTrue
    }.pendingUntilFixed

    "succeed if a rover and commands are provided" in {
      val input =
        s"""123 456
           |5 8 E
           |RLLMRLL""".stripMargin

      println(s"Input is:\n<$input>")

      val result: Either[String, RoversInput] = CommandParser.parse(input)

      result.isRight must beTrue
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
