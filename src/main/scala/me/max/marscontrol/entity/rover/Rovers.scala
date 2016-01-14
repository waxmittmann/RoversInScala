package me.max.marscontrol.entity.rover

import me.max.marscontrol.entity.rover.Rovers.PlateauDimensions
import me.max.marscontrol.entity.Command

object Rovers {
  type PlateauDimensions = (Int, Int)
  type RoverStateWithOutput = (Rovers, List[String])
  type RoverStateAndCommands = (RoverPositionOrientation, List[Command])
  type RoversInput = (PlateauDimensions, List[RoverPositionOrientation], List[List[Command]])
//  type RoversInput = (PlateauDimensions, List[(RoverPositionOrientation, List[Command])])
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
      println("Non-matching shapes for:\n" + rovers + "\n" + command)
      Left(RoverAndCommandShapesDontMatch)
    } else {
      Right[RoverError, Rovers](Rovers(plateau, runCommands))
    }
    result
  }

  def add(roverPositionOrientation: RoverPositionOrientation): Rovers = {
    Rovers(plateau, roverPositionOrientation :: rovers)
  }

  override def toString(): String = {
    val plateauStr = s"Plateau: ${plateau._1} ${plateau._2}"
    plateauStr + "\n" + rovers.map(rover => s"Position: ${rover.position}, Orientation: ${rover.orientation}")
  }
}

