package me.max.rover.entity

import Orientation.{East, North, South, West}

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