package me.max.marscontrol.entity.rover

import me.max.marscontrol.entity.rover.Rovers.PlateauDimensions
import me.max.marscontrol.entity.Command

object Rovers {
  type PlateauDimensions = (Int, Int)
  type RoverStateWithOutput = (Rovers, List[String])
  type RoverStateAndCommands = (RoverPositionOrientation, List[Command])
}

case class RoversInput(plateauDimensions: PlateauDimensions, rovers: List[RoverPositionOrientation],
                       commands: List[List[Command]])

case class Rovers(plateau: PlateauDimensions, rovers: List[RoverPositionOrientation]) {
  def execute(command: List[Command]): Either[RoverError, Rovers] = {
    def runCommands: List[RoverPositionOrientation] = {
      rovers.zip(command)
        .foldRight(List[RoverPositionOrientation]()) {
          case ((rover, command), list) => command(rover) :: list
        }
    }

    val result: Either[RoverError, Rovers] = if (rovers.size != command.size) {
      Left(RoverAndCommandShapesDontMatch)
    } else {
      Right(Rovers(plateau, runCommands))
    }
    result
  }

  def add(roverPositionOrientation: RoverPositionOrientation): Rovers = {
    Rovers(plateau, roverPositionOrientation :: rovers)
  }

  override def toString(): String = {
    val plateauStr = s"Plateau: ${plateau._1} ${plateau._2}"
    plateauStr + "\n" + rovers.foldRight("")((rover, str) => s"$str\nPosition: ${rover.position}, Orientation: ${rover.orientation}")
  }
}

