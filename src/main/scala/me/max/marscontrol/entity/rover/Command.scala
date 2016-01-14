package me.max.marscontrol.entity

import me.max.marscontrol.entity.Orientation.{East, South, West, North}
import me.max.marscontrol.entity.rover.RoverPositionOrientation

object Command {
  def parse(commandChar: Char): Option[Command] = {
    commandChar match {
      case 'L' => Some(TurnLeft)
      case 'R' => Some(TurnRight)
      case 'M' => Some(Move)
      case _ => None
    }
  }
}

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

case object Noop extends Command {
  def apply(rover: RoverPositionOrientation) = rover
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