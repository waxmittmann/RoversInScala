package me.max.marscontrol.entity.rover

sealed trait RoverError
case object RoverAndCommandShapesDontMatch extends RoverError
case class ValidationError(msg: List[String]) extends RoverError
