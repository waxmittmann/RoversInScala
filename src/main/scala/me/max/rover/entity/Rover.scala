package me.max.rover.entity

import me.max.rover.entity.Orientation.Orientation

object Rovers {
  type PlateauDimensions = (Int, Int)
  type RoverStateWithOutput = (Rovers, List[String])
  type RoverStateAndCommands = (RoverPositionOrientation, List[Command])
  type RoversInput = (PlateauDimensions, List[RoverStateAndCommands])

  def empty(plateauDimensions: PlateauDimensions) = Rovers(plateauDimensions, List.empty)
}

sealed trait RoverError
case object RoverAndCommandShapesDontMatch extends RoverError

object RoversAccumulator {
  def apply(rovers: Rovers): RoversAccumulator = RoversAccumulator(Right(rovers, List()))
}

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

case class RoverPositionOrientation(position: Position, orientation: Orientation) {
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
