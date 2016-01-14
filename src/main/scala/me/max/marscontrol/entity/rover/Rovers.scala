package me.max.marscontrol.entity.rover

import me.max.marscontrol.entity.rover.Rovers.PlateauDimensions
import me.max.marscontrol.entity.Command

object Rovers {
  type PlateauDimensions = (Int, Int)
  type RoverStateWithOutput = (Rovers, List[String])
  type RoverStateAndCommands = (RoverPositionOrientation, List[Command])
  type RoversInput = (PlateauDimensions, List[RoverStateAndCommands])

  def empty(plateauDimensions: PlateauDimensions) = Rovers(plateauDimensions, List.empty)
}

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
      Right[RoverError, Rovers](Rovers(plateau, runCommands))
    }
    result
  }

  def add(roverPositionOrientation: RoverPositionOrientation): Rovers = {
    Rovers(plateau, roverPositionOrientation :: rovers)
  }
}

