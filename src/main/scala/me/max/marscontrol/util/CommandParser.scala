package me.max.marscontrol.util

import java.io.Serializable

import me.max.marscontrol.entity.{Noop, Position, Orientation, Command}
import me.max.marscontrol.entity.Orientation.Orientation
import me.max.marscontrol.entity.rover.{RoverPositionOrientation}
import me.max.marscontrol.entity.rover.Rovers.{PlateauDimensions, RoversInput}

/** *

This plateau, which is curiously rectangular, must be navigated by the rovers so that their on board cameras can get a complete view of the surrounding terrain to send back to Earth.
A rover's position is represented by a combination of an x and y co-ordinates and a letter representing one of the four cardinal compass points. The plateau is divided up into a grid to simplify navigation. An example position might be 0, 0, N, which means the rover is in the bottom left corner and facing North.

In order to control a rover, NASA sends a simple string of letters. The possible letters are 'L', 'R' and 'M'. 'L' and 'R' makes the rover spin 90 degrees left or right respectively, without moving from its current spot.
'M' means move forward one grid point, and maintain the same heading.
Assume that the square directly North from (x, y) is (x, y+1).

Input (whether hard coded or input from keyboard):
The first line of input is the upper-right coordinates of the plateau, the lower-left coordinates are assumed to be 0,0.
The rest of the input is information pertaining to the rovers that have been deployed. Each rover has two lines of input. The first line gives the rover's position, and the second line is a series of instructions telling the rover how to explore the plateau.

The position is made up of two integers and a letter separated by spaces, corresponding to the x and y co-ordinates and the rover's orientation.
Each rover will be finished Listuentially, which means that the second rover won't start to move until the first one has finished moving.
Output:
The output for each rover should be its final co-ordinates and heading.

Plateau max X and Y, Starting coordinates, direction and path for two rovers:
5 5
1 2 N
LMLMLMLMM
3 3 E
MMRMMRMRRM

Output and new coordinates:
1 3 N
5 1 E

  */

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
      if (input.length == 0) {
        return expectedLine match {
          case RoverDefinitionOrEnd(a, b) => Right(ParserState(CompletedState(a, b), List()))
          case _ => Left(s"No more commands left, expected $expectedLine")
        }
      }

      val result: Either[String, ParserState] = expectedLine match {
        case PlateauDefinition => {
          try {
            val parts = input.head.split(" ")
            val (width, height) = (parts(0).toInt, parts(1).toInt)
            Right(ParserState(RoverDefinitionOrEnd(
              (width, height),
              List[(RoverPositionOrientation, List[Command])]()), input.tail))
          } catch {
            case _ => Left(s"Failed to parse line '${input.head}'")
          }

          //val date = """(\d\d\d\d)-(\d\d)-(\d\d)""".r
          //          val plateauRegex = """(\d+) (\d+)""".r
          //          val result: Either[String, (Int, Int)] = plateauRegex.findAllIn(input.head) {
          //            (width, height) => Right((Integer.parseInt(width), Integer.parseInt(height)))
          //            _ => Left(s"Could not parse plateau from ${input.head}")
          //          }
        }
        case RoverDefinitionOrEnd(dimensions, roversSoFar) => {
          //          val plateauRegex = """(\d+) (\d+) (N|S|E|W)""".r
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
        case RoverCommands(dimensions, currentPositionOrientation, roversSoFar) => {
          //val roverCommandsRegex = """(M|R|L)*""".r
          val r: Option[List[Command]] = input.head
              .foldRight[Option[List[Command]]](Some(List[Command]()))((curCommandChar, commands) => {
            val r: Option[List[Command]] = commands.flatMap((soFar:List[Command]) => {
              val pr:Option[Command] = Command.parse(curCommandChar)
              val r = pr.fold[Option[List[Command]]](None)((in) => Some(in :: soFar))
              r
            })
            r
          })
          val r2: Either[String, List[Command]] = r.fold[Either[String, List[Command]]](Left(s"Failed to parse line '${input.head}'"))(Right(_))
          //          RoverDefinitionOrEnd(plateauDimensions,  r2
          val r3: Either[String, ParserState] = r2.right
            .map(commands => ParserState(RoverDefinitionOrEnd(dimensions, (currentPositionOrientation, commands) :: roversSoFar), input.tail))
          r3
        }
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
      val r: Either[String, RoversInput] = parsedInput.right
        .map((in: ((Int, Int), List[(RoverPositionOrientation, List[Command])])) => {
          val rovers = in._2
//          val longestList = rovers.map(_._2).max.length
          //Todo: Replace with max
          val longestList = rovers.map(_._2).foldLeft(0)((max, li) => {
            if (li.length > max)
              li.length
            else
              max
          })
          val (positionsOrientations, commands) = rovers.unzip
          val paddedCommands: List[List[Command]] = commands.map(li => li.padTo(longestList, Noop))
//          val r: ((Int, Int), List[RoverPositionOrientation], List[List[Command]]) = (in._1, positionsOrientations, paddedCommands)
          val r: RoversInput = (in._1, positionsOrientations, paddedCommands)
          r
        })
      r
    }
//
//      var curState = ParserState(PlateauDefinition, input.split("\n").toList)
//      while (true) {
//        val result = parseH(curState)
//
//      }
//    }
}