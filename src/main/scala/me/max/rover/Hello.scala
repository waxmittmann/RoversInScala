package me.max.rover

import me.max.rover.RoversTypes.{RoversInput, PlateauDimensions}

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

object RoversTypes {
  type PlateauDimensions = (Int, Int)

  type RoverStateWithOutput = (Rovers, List[String])

  type RoverStateAndCommands = (RoverPositionOrientation, List[Command])

  type RoversInput = (PlateauDimensions, List[RoverStateAndCommands])
}

sealed trait Orientation

case object North extends Orientation
case object South extends Orientation
case object West extends Orientation
case object East extends Orientation

sealed trait Command {
  def apply(rover: RoverPositionOrientation): RoverPositionOrientation
}

case object TurnLeft extends Command {
  def apply(rover: RoverPositionOrientation) = {
    rover.mapOrientation({
      case North => West
      case West => South
      case South => East
      case East => North
    })
  }
}
case object TurnRight extends Command {
  def apply(rover: RoverPositionOrientation) = {
    rover.mapOrientation({
      case North => East
      case West => North
      case South => West
      case East => South
    })
  }
}

case object Move extends Command {
  def apply(rover: RoverPositionOrientation) = {
    rover.map((curPositionOrientation) => {
      (curPositionOrientation.orientation) match {
        case North => curPositionOrientation.mapPosition(_.increment(0, 1))
        case South => curPositionOrientation.mapPosition(_.increment(0, -1))
        case West => curPositionOrientation.mapPosition(_.increment(-1, 0))
        case East => curPositionOrientation.mapPosition(_.increment(1, 0))
      }
    })
  }
}

class CommandParser() {
  def parse(input: String): RoversInput = ???
}

case class Position(x: Int, y: Int) {
  def increment(xi: Int, yi: Int) = Position(x + xi, y + yi)
}

object Rovers {
  def empty(plateauDimensions: PlateauDimensions) = Rovers(plateauDimensions, List.empty)
}

object RoversAccumulator {
  def apply(rovers: Rovers): RoversAccumulator = RoversAccumulator(Right(rovers, List()))
}

sealed trait RoverError
case object RoverAndCommandShapesDontMatch extends RoverError

case class RoversAccumulator(state: Either[(RoverError, List[Rovers]), (Rovers, List[Rovers])]) {
  def execute(command: List[Command]): RoversAccumulator = {
    state.fold((_) => this, cur => RoversAccumulator({
      cur._1.execute(command).fold[Either[(RoverError, List[Rovers]), (Rovers, List[Rovers])]](
        error => Left((error, cur._2)), result => Right((result, result :: cur._2)))
    }))
  }
}

case class Rovers(plateau: PlateauDimensions, rovers: List[RoverPositionOrientation]) {
  def execute(command: List[Command]): Either[RoverError, Rovers] = {
    def runCommands: List[RoverPositionOrientation] = {
      val li: List[RoverPositionOrientation] = rovers.zip(command)
        .foldLeft(List[RoverPositionOrientation]()) {
          case (list, (rover, command)) => command(rover) :: list
        }
      li
    }

    val result: Either[RoverError, Rovers] = if (rovers.size != command.size) {
      Left(RoverAndCommandShapesDontMatch)
    } else {
      Right[RoverError, Rovers](Rovers(plateau, runCommands))
    }
    result
  }

  def add(roverPositionOrientation: RoverPositionOrientation): Rovers = {
    Rovers(plateau, roverPositionOrientation :: rovers)
  }

  def changePlateau(plateauDimensions: PlateauDimensions): Rovers = ???
}

object Orientation {
  def fromString(strRep: String): Option[Orientation] = {
    strRep match {
      case "N" => Some(North)
      case "W" => Some(West)
      case "E" => Some(East)
      case "S" => Some(South)
      case _ => None
    }
  }
}

case class RoverPositionOrientation (position: Position, orientation: Orientation) {
  def mapPosition(f: Position => Position): RoverPositionOrientation = {
    RoverPositionOrientation(f(position), orientation)
  }

  def mapOrientation(f: (Orientation) => Orientation): RoverPositionOrientation = {
    RoverPositionOrientation(position, f(orientation))
  }

  def map(f: (RoverPositionOrientation) => RoverPositionOrientation): RoverPositionOrientation = {
    f(this)
  }
}


object Hello {
  def main(args: Array[String]): Unit = {
    val input =
      """
        |5 5
        |1 2 N
        |LMLMLMLMM
        |3 3 E
        |MMRMMRMRRM
      """.stripMargin

      val initialRovers = Rovers.empty((5, 5))
        .add(RoverPositionOrientation(Position(1, 1), North))
        .add(RoverPositionOrientation(Position(1, 2), East))

      val finalState = RoversAccumulator(initialRovers)
          .execute(List(Move, TurnLeft))
          .execute(List(Move, TurnLeft))
          .execute(List(Move, TurnLeft))
          .execute(List(Move, TurnLeft))
          .state

    println(finalState)
  }
}
