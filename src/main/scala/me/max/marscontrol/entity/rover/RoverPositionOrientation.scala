package me.max.marscontrol.entity.rover

import me.max.marscontrol.entity.Orientation.Orientation
import me.max.marscontrol.entity.Position

object RoverPositionOrientation {
  def apply(x: Int, y: Int, o: Orientation): RoverPositionOrientation = {
    RoverPositionOrientation(Position(x, y), o)
  }
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
