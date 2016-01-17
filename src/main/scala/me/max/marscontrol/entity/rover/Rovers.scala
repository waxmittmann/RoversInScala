package me.max.marscontrol.entity.rover

import me.max.marscontrol.entity.rover.Rovers.PlateauDimensions
import me.max.marscontrol.entity.Command

object Rovers {
  type PlateauDimensions = (Int, Int)
  type RoverStateWithOutput = (Rovers, List[String])
  type RoverStateAndCommands = (RoverPositionOrientation, List[Command])
}

case class RoversInput(rovers: Rovers, commands: List[List[Command]])

case class Rovers(plateau: PlateauDimensions, rovers: List[RoverPositionOrientation] = List(), constraints: List[Constraint] = List()) {
  def execute(command: List[Command]): Either[RoverError, Rovers] = {
    def runCommands: List[RoverPositionOrientation] = {
      rovers.zip(command)
        .foldRight(List[RoverPositionOrientation]()) {
          case ((rover, command), list) => command(rover) :: list
        }
    }

    def validate(rovers: Rovers): Either[RoverError, Rovers] = {
      val errors = constraints.foldLeft(List[String]())((li, constraint)
          => constraint.test(rovers).fold(error => error :: li, _ => li))

      if (errors.length > 0) {
        Left(ValidationError(errors))
      } else {
        Right(rovers)
      }
    }

    val result: Either[RoverError, Rovers] = if (rovers.size != command.size) {
      Left(RoverAndCommandShapesDontMatch)
    } else {
      val newState: Rovers = Rovers(plateau, runCommands, constraints)
      validate(newState)
    }
    result
  }

  def addRover(roverPositionOrientation: RoverPositionOrientation): Rovers = {
    Rovers(plateau, roverPositionOrientation :: rovers, constraints)
  }

  def addConstraint(constraint: Constraint): Rovers = {
    Rovers(plateau, rovers, constraint :: constraints)
  }

  override def toString(): String = {
    val plateauStr = s"Plateau: ${plateau._1} ${plateau._2}"
    plateauStr + " " + rovers.foldRight("")((rover, str) =>
      s"Position: ${rover.position}, Orientation: ${rover.orientation}\n$str")
  }
}

