package me.max.marscontrol.parser

import me.max.marscontrol.entity.Orientation.Orientation
import me.max.marscontrol.entity.{Orientation, Position, Command}
import me.max.marscontrol.entity.Orientation.Orientation
import me.max.marscontrol.entity.rover.RoverPositionOrientation
import me.max.marscontrol.entity.rover.Rovers._

sealed trait ParserState {
  val input: List[String]

  def process(): Either[String, ParserState]

  def hasMore() = {
    input.length > 0
  }
}

//Parse the initial definition of the plateau, transitions to RoverDefinitionOrEnd
case class PlateauDefinition(input: List[String]) extends ParserState {
  val dimensionsRegex = """^(\d+) (\d+)$""".r

  override def process(): Either[String, ParserState] = {
    if (!hasMore()) {
      return Left("Failed to parse plateau definition, no input")
    }

    dimensionsRegex.findFirstMatchIn(input.head).fold[Either[String, ParserState]](
        Left(s"Failed to parse plateau definition, error in line '${input.head}'")
      ){ result =>
        val (width, height) = (result.group(1).toInt, result.group(2).toInt)
        Right(RoverDefinitionOrEnd(input.tail, (width, height), List()))
      }
  }
}

//Parse a rover; if no input is left, we can terminate here, else transitions to RoverCommands
case class RoverDefinitionOrEnd(input: List[String], plateauDimensions: PlateauDimensions,
                                roversSoFar: List[(RoverPositionOrientation, List[Command])]) extends ParserState {
  val roverPattern = """^(\d+) (\d+) (N|W|E|S)$""".r

  override def process(): Either[String, ParserState] = {
    def buildNextState(width: Int, height: Int)(orientation:Orientation): ParserState = {
      RoverCommands(input.tail, plateauDimensions,
        RoverPositionOrientation(Position(width, height), orientation), roversSoFar)
    }

    if (!hasMore()) {
      Right(this)
    } else {
      //Todo: Check it only matches once
      roverPattern.findFirstMatchIn(input.head).fold[Either[String, ParserState]](
          Left(s"Failed to parse rover definition, error in line '${input.head}'")
        ){ result =>
          val (width, height, orientationString) = (result.group(1).toInt, result.group(2).toInt, result.group(3))

          Orientation.fromString(orientationString).fold[Either[String, ParserState]](
            Left(s"Failed to parse rover definition, error in line '${input.head}' in action"))(
            orientation => Right(buildNextState(width, height)(orientation)))
        }
    }
  }
}

//Parse a rover's commands, tranistions back to RoverDefinitionOrEnd
case class RoverCommands(input: List[String], plateauDimensions: PlateauDimensions,
                         currentRoverPositionOrientation: RoverPositionOrientation,
                         roversSoFar: List[(RoverPositionOrientation, List[Command])]) extends ParserState {
  override def process(): Either[String, ParserState] = {
    def parseCommand(curCommandChar: Char): Option[Command] = {
      Command.parse(curCommandChar).fold[Option[Command]](None)(Some(_))
    }

    def buildNextState(commands: List[Command]): ParserState = {
      RoverDefinitionOrEnd(input.tail, plateauDimensions, (currentRoverPositionOrientation, commands) :: roversSoFar)
    }

    if (!hasMore()) {
      Left("Failed to parse command definition, no input")
    } else {
      val commandsList: Option[List[Command]] =
        input.head.foldRight[Option[List[Command]]](Some(List[Command]()))((curCommandChar, commands) => {
            commands.flatMap((soFar:List[Command]) => parseCommand(curCommandChar).map(_ :: soFar))
        })

      val commandsResult = commandsList.fold[Either[String, List[Command]]](
        Left(s"Failed to parse command definition, error in line '${input.head}'"))(
        Right(_))

      val nextStateOrError = commandsResult.right.map(buildNextState)
      nextStateOrError
    }
  }
}
