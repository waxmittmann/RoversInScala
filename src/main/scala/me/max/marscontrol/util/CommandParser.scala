package me.max.marscontrol.util

import java.io.Serializable

import me.max.marscontrol.entity.{Noop, Position, Orientation, Command}
import me.max.marscontrol.entity.Orientation.Orientation
import me.max.marscontrol.entity.rover.{RoverPositionOrientation}
import me.max.marscontrol.entity.rover.Rovers.{PlateauDimensions, RoversInput}
import me.max.marscontrol.util.CommandParser.{CompletedState, ParserState}

//Needs big refactors
object CommandParser {

  sealed trait ExpectedLine {
    def process(): Either[String, ExpectedLine]
    def hasMore(): Boolean
  }

  case class PlateauDefinition(input: List[String]) extends ExpectedLine {
    override def process(): Either[String, ExpectedLine] = {
    if (input.length < 3) {
      return Left("Not enough input left to finish")
    }

    try {
        val parts = input.head.split(" ")
        val (width, height) = (parts(0).toInt, parts(1).toInt)
        RoverDefinitionOrEnd(input.tail, (width, height),
          List[(RoverPositionOrientation, List[Command])]()).process()
      } catch {
        case _ => Left(s"Failed to parse line '${input.head}'")
      }
    }

    def hasMore() = {
      input.length > 0
    }
  }

  case class RoverDefinitionOrEnd(input: List[String], plateauDimensions: PlateauDimensions,
                                  roversSoFar: List[(RoverPositionOrientation, List[Command])]) extends ExpectedLine {
    override def process(): Either[String, ExpectedLine] = {
      if (input.length == 0) {
        return Right(this)
      } else if (input.length < 2) {
        return Left("Not enough input left to finish")
      }

      try {
        val parts = input.head.split(" ")
        val (width, height, orientationString) = (parts(0).toInt, parts(1).toInt, parts(2))
//        Orientation.fromString(orientationString)
//            .fold[Either[String, ExpectedLine]](
//              Left(s"Failed to parse line '${input.head}'"))(
//              (orientation:Orientation) => Right(RoverCommands(input.tail, plateauDimensions,
//                RoverPositionOrientation(Position(width, height), orientation), roversSoFar)))

        Orientation.fromString(orientationString)
            .fold[Either[String, ExpectedLine]](
              Left(s"Failed to parse line '${input.head}'"))(
              (orientation:Orientation) => RoverCommands(input.tail, plateauDimensions,
                RoverPositionOrientation(Position(width, height), orientation), roversSoFar).process())

      } catch {
        case _ => Left(s"Failed to parse line '${input.head}'")
      }
    }

    def hasMore() = {
      input.length > 0
    }
  }

  case class RoverCommands(input: List[String], plateauDimensions: PlateauDimensions,
                           currentRoverPositionOrientation: RoverPositionOrientation,
                           roversSoFar: List[(RoverPositionOrientation, List[Command])]) extends ExpectedLine {
    override def process(): Either[String, ExpectedLine] = {
      val listOfCommandsO: Option[List[Command]] = input.head
              .foldRight[Option[List[Command]]](Some(List[Command]()))((curCommandChar, commands) => {
            commands.flatMap((soFar:List[Command]) => {
              Command.parse(curCommandChar).fold[Option[List[Command]]](None)((in) => Some(in :: soFar))
            })
          })

      val result: Either[String, ExpectedLine] = listOfCommandsO
          .fold[Either[String, List[Command]]](Left(s"Failed to parse line '${input.head}'"))(Right(_)).right
        .flatMap(commands =>
          RoverDefinitionOrEnd(input.tail, plateauDimensions, (currentRoverPositionOrientation, commands) :: roversSoFar).process()
        )
      result
    }

    def hasMore() = {
      input.length > 0
    }
  }

  case class CompletedState(plateauDimensions: PlateauDimensions,
                            roversSoFar: List[(RoverPositionOrientation, List[Command])]) extends ExpectedLine {
    override def process() = ???

    def hasMore() = {
      false
    }
  }

  case class ParserState(expectedLine: ExpectedLine) {
    def parse(): Either[String, ExpectedLine] = {
      println("Inside")
      if (!expectedLine.hasMore()) {
        println("No more")
        return expectedLine match {
          case RoverDefinitionOrEnd(_, a, b) => Right(CompletedState(a, b))
          case _ => Left(s"No more commands left, expected $expectedLine")
        }
      }
//
//      println(input.length + " left")
//      if (input.length == 0) {
//        println("Returning")
//        return expectedLine match {
////          case (RoverDefinitionOrEnd(_) => Right(CompletedState(a, b))
//          case RoverDefinitionOrEnd(_, a, b) => Right(CompletedState(a, b))
//          case _ => Left(s"No more commands left, expected $expectedLine")
//        }
//      }

      expectedLine.process()


//      val result: Either[String, ParserState] = expectedLine match {
//        case PlateauDefinition => processPlateauDefinition()
//        case RoverDefinitionOrEnd(dimensions, roversSoFar)
//          => processRoverDefinition(dimensions, roversSoFar)
//        case RoverCommands(dimensions, currentPositionOrientation, roversSoFar)
//          => processRoverCommands(dimensions, currentPositionOrientation, roversSoFar)
//      }
//      result.right.flatMap(_.parse())
    }
  }

  //Need to get rid of ParserState
    def parse(input: String): Either[String, RoversInput] = {
      def parseInput(): Either[String, ((Int, Int), List[(RoverPositionOrientation, List[Command])])] = {
        PlateauDefinition(input.split("\n").toList).process() match {
          case Right(RoverDefinitionOrEnd(_, dimensions, rovers)) => Right((dimensions, rovers))
          case Right(state) => Left(s"Parser error, incorrect state: $state")
          case Left(error: String) => Left(error)
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