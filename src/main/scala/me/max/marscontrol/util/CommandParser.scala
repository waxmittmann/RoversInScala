package me.max.marscontrol.util

import java.io.Serializable

import me.max.marscontrol.entity.{Noop, Position, Orientation, Command}
import me.max.marscontrol.entity.Orientation.Orientation
import me.max.marscontrol.entity.rover.{RoverPositionOrientation}
import me.max.marscontrol.entity.rover.Rovers.{PlateauDimensions, RoversInput}

object CommandParser {

  sealed trait ExpectedLine

  case object PlateauDefinition extends ExpectedLine

  case class RoverDefinitionOrEnd(plateauDimensions: PlateauDimensions,
                                  roversSoFar: List[(RoverPositionOrientation, List[Command])]) extends ExpectedLine

  case class RoverCommands(plateauDimensions: PlateauDimensions,
                           currentRoverPositionOrientation: RoverPositionOrientation,
                           roversSoFar: List[(RoverPositionOrientation, List[Command])]) extends ExpectedLine

  case class CompletedState(plateauDimensions: PlateauDimensions,
                            roversSoFar: List[(RoverPositionOrientation, List[Command])]) extends ExpectedLine

  case class ParserState(expectedLine: ExpectedLine, input: List[String]) {
    def parse(): Either[String, ParserState] = {
      def processPlateauDefinition() = {
        try {
          val parts = input.head.split(" ")
          val (width, height) = (parts(0).toInt, parts(1).toInt)
          Right(ParserState(RoverDefinitionOrEnd(
            (width, height),
            List[(RoverPositionOrientation, List[Command])]()), input.tail))
        } catch {
          case _ => Left(s"Failed to parse line '${input.head}'")
        }
      }

      def processRoverDefinition(dimensions: PlateauDimensions, roversSoFar: List[(RoverPositionOrientation, List[Command])]) = {
        try {
          val parts = input.head.split(" ")
          val (width, height, orientationString) = (parts(0).toInt, parts(1).toInt, parts(2))
          Orientation.fromString(orientationString)
            .fold[Either[String, ParserState]](
              Left(s"Failed to parse line '${input.head}'"))(
              (orientation:Orientation) => Right(ParserState(RoverCommands(dimensions,
                RoverPositionOrientation(Position(width, height), orientation), roversSoFar), input.tail)))
        } catch {
          case _ => Left(s"Failed to parse line '${input.head}'")
        }
      }

      def processRoverCommands(dimensions: PlateauDimensions, currentPositionOrientation: RoverPositionOrientation,
                               roversSoFar: List[(RoverPositionOrientation, List[Command])]) = {
        val listOfCommandsO: Option[List[Command]] = input.head
              .foldRight[Option[List[Command]]](Some(List[Command]()))((curCommandChar, commands) => {
            commands.flatMap((soFar:List[Command]) => {
              Command.parse(curCommandChar).fold[Option[List[Command]]](None)((in) => Some(in :: soFar))
            })
          })

        val result: Either[String, ParserState] = listOfCommandsO
          .fold[Either[String, List[Command]]](Left(s"Failed to parse line '${input.head}'"))(Right(_)).right
          .map(commands => ParserState(RoverDefinitionOrEnd(dimensions, (currentPositionOrientation, commands) :: roversSoFar), input.tail))
        result
      }

      if (input.length == 0) {
        return expectedLine match {
          case RoverDefinitionOrEnd(a, b) => Right(ParserState(CompletedState(a, b), List()))
          case _ => Left(s"No more commands left, expected $expectedLine")
        }
      }

      val result: Either[String, ParserState] = expectedLine match {
        case PlateauDefinition => processPlateauDefinition()
        case RoverDefinitionOrEnd(dimensions, roversSoFar)
          => processRoverDefinition(dimensions, roversSoFar)
        case RoverCommands(dimensions, currentPositionOrientation, roversSoFar)
          => processRoverCommands(dimensions, currentPositionOrientation, roversSoFar)
      }
      result.right.flatMap(_.parse())
    }
  }

    def parse(input: String): Either[String, RoversInput] = {
      def parseInput(): Either[String, ((Int, Int), List[(RoverPositionOrientation, List[Command])])] = {
        new ParserState(PlateauDefinition, input.split("\n").toList).parse match {
          case Right(ParserState(CompletedState(dimensions, rovers), _)) => Right((dimensions, rovers))
          case Right(ParserState(state, _)) => Left(s"Parser error, incorrect state: $state")
          case Left(error) => Left(error)
        }
      }

      val parsedInput: Either[String, ((Int, Int), List[(RoverPositionOrientation, List[Command])])] = parseInput()
      parsedInput.right
        .map((in: ((Int, Int), List[(RoverPositionOrientation, List[Command])])) => {
          val rovers = in._2
          //Todo: Replace with max
          val longestList = rovers.map(_._2).foldLeft(0)((max, li) => {
            if (li.length > max)
              li.length
            else
              max
          })
          val (positionsOrientations, commands) = rovers.unzip
          val paddedCommands: List[List[Command]] = commands.map(li => li.padTo(longestList, Noop))
          (in._1, positionsOrientations, paddedCommands)
        })
    }
}