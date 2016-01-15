package me.max.marscontrol.util

import java.io.Serializable

import me.max.marscontrol.entity.{Noop, Position, Orientation, Command}
import me.max.marscontrol.entity.Orientation.Orientation
import me.max.marscontrol.entity.rover.{RoversInput, RoverPositionOrientation}
import me.max.marscontrol.entity.rover.Rovers.{PlateauDimensions}

import scala.annotation.tailrec
import scala.util.matching.Regex.Match

object CommandParser {

  sealed trait ParserState {
    val input: List[String]

    def process(): Either[String, ParserState]

    def hasMore() = {
      input.length > 0
    }
  }

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

  case class RoverDefinitionOrEnd(input: List[String], plateauDimensions: PlateauDimensions,
                                  roversSoFar: List[(RoverPositionOrientation, List[Command])]) extends ParserState {
    val roverPattern = """^(\d+) (\d+) (N|W|E|S)$""".r

    override def process(): Either[String, ParserState] = {
      if (!hasMore()) {
        return Right(this)
      }

      //Todo: Check it only matches once
      roverPattern.findFirstMatchIn(input.head).fold[Either[String, ParserState]](
        Left(s"Failed to parse rover definition, error in line '${input.head}'")
      ){ result =>
        val (width, height, orientationString) = (result.group(1).toInt, result.group(2).toInt, result.group(3))
        Orientation.fromString(orientationString)
          .fold[Either[String, ParserState]](
            Left(s"Failed to parse rover definition, error in line '${input.head}' in action"))(
            (orientation:Orientation) => Right(RoverCommands(input.tail, plateauDimensions,
              RoverPositionOrientation(Position(width, height), orientation), roversSoFar)))
      }
    }
  }

  //Clean up sphagetti code
  case class RoverCommands(input: List[String], plateauDimensions: PlateauDimensions,
                           currentRoverPositionOrientation: RoverPositionOrientation,
                           roversSoFar: List[(RoverPositionOrientation, List[Command])]) extends ParserState {
    override def process(): Either[String, ParserState] = {
      if (!hasMore()) {
        return Left("Failed to parse command definition, no input")
      }

      val listOfCommandsO: Option[List[Command]] =
        input.head.foldRight[Option[List[Command]]](Some(List[Command]()))((curCommandChar, commands) => {
            commands.flatMap((soFar:List[Command]) => {
              Command.parse(curCommandChar).fold[Option[List[Command]]](None)((in) => Some(in :: soFar))
            })
          })

      val result: Either[String, ParserState] = listOfCommandsO
          .fold[Either[String, List[Command]]](Left(s"Failed to parse command definition, " +
              s"error in line '${input.head}'"))(Right(_)).right
        .map(commands =>
          RoverDefinitionOrEnd(input.tail, plateauDimensions, (currentRoverPositionOrientation, commands) :: roversSoFar)
        )
      result
    }
  }

  def parse(input: String): Either[String, RoversInput] = {
    //Todo: Replace with max
    def longestCommandListLength(rovers: List[(RoverPositionOrientation, List[Command])]): Int = {
      val longestList = rovers.map(_._2).foldLeft(0)((max, li) => {
        if (li.length > max)
          li.length
        else
          max
      })
      longestList
    }

    val parseResult: Either[String, ParserState] = doParse(input)
    val parsedInput: Either[String, ((Int, Int), List[(RoverPositionOrientation, List[Command])])] = transformResult(parseResult)

    //Clean up
    parsedInput.right
      .map((in: ((Int, Int), List[(RoverPositionOrientation, List[Command])])) => {
        val rovers = in._2
        val longestList = longestCommandListLength(rovers)
        val (positionsOrientations, commands) = rovers.unzip
        val paddedCommands: List[List[Command]] = commands.map(li => li.padTo(longestList, Noop))
        RoversInput(in._1, positionsOrientations, paddedCommands)
      })
  }

  private def doParse(input: String): Either[String, ParserState] = {
    @tailrec
    def go(expectedLine: ParserState): Either[String, ParserState] = {
      val result: Either[String, ParserState] = expectedLine.process()

      //So nasty =_( But is there an easy way (no trampolining) to make this FP-tailrecursive?
      if (result.isLeft || !result.right.get.hasMore()) {
        result
      } else {
        go(result.right.get)
      }
    }

    go(PlateauDefinition(input.split("\n").toList))
  }

  private def transformResult(result: Either[String, ParserState]):
                      Either[String, ((Int, Int), List[(RoverPositionOrientation, List[Command])])] = {
    result match {
      case Right(RoverDefinitionOrEnd(_, dimensions, rovers)) => Right((dimensions, rovers))
      case Right(state) => Left(s"Parser error, incorrect state: $state")
      case Left(error: String) => Left(error)
    }
  }
}