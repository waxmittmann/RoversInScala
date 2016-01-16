package me.max.marscontrol.entity.rover

sealed trait RoverError
case object RoverAndCommandShapesDontMatch extends RoverError
