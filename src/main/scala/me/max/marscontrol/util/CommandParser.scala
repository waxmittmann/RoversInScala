package me.max.marscontrol.util

import java.io.Serializable

import me.max.marscontrol.entity.{Noop, Position, Orientation, Command}
import me.max.marscontrol.entity.Orientation.Orientation
import me.max.marscontrol.entity.rover.{RoverPositionOrientation}
import me.max.marscontrol.entity.rover.Rovers.{PlateauDimensions, RoversInput}

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

  //Clean up sphagetti code
  case class RoverCommands(input: List[String], plateauDimensions: PlateauDimensions,
                           currentRoverPositionOrientation: RoverPositionOrientation,
                           roversSoFar: List[(RoverPositionOrientation, List[Command])]) extends ExpectedLine {
    override def process(): Either[String, ExpectedLine] = {
      val listOfCommandsO: Option[List[Command]] =
        input.head.foldRight[Option[List[Command]]](Some(List[Command]()))((curCommandChar, commands) => {
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

  def parse(input: String): Either[String, RoversInput] = {
    def parseInput(): Either[String, ((Int, Int), List[(RoverPositionOrientation, List[Command])])] = {
      PlateauDefinition(input.split("\n").toList).process() match {
        case Right(RoverDefinitionOrEnd(_, dimensions, rovers)) => Right((dimensions, rovers))
        case Right(state) => Left(s"Parser error, incorrect state: $state")
        case Left(error: String) => Left(error)
      }
    }

    //Clean up
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