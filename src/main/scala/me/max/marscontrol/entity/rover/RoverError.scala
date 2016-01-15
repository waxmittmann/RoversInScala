package me.max.marscontrol.entity.rover

import scala.util.matching.Regex.{Match, MatchIterator}

sealed trait RoverError
case object RoverAndCommandShapesDontMatch extends RoverError

object Main {
  def main(args: Array[String]) = {
    val pattern = "^hello (.*) my (.*) friend$".r
//    val result: MatchIterator = pattern.findAllIn("hello saray my pretty pickle friend")
    val result = pattern.findAllMatchIn("hello saray my pretty pickle friend")
//    val result = pattern.findAllMatchIn("hello saray my pretty pickle friend SNOOT")


    result.foreach { (m: Match) =>
      println(m.group(1) + ", " + m.group(2))
    }

  }
}