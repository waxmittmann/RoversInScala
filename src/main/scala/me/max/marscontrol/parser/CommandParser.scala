package me.max.marscontrol.parser

import me.max.marscontrol.entity.rover.{Rovers, RoverPositionOrientation, RoversInput}
import me.max.marscontrol.entity.{Command, Noop}

import scala.annotation.tailrec

object CommandParser {
  def parse(input: String): Either[String, RoversInput] = {
    //Todo: Replace with max
    def longestCommandListLength(rovers: List[(RoverPositionOrientation, List[Command])]): Int = {
      val longestList = rovers.map(_._2).foldLeft(0)((max, li) => {
        if (li.length > max)
          li.length
        else
          max
      })
      longestList
    }

    val parseResult: Either[String, ParserState] = doParse(input)
    val parsedInput: Either[String, RoversInput] = transformResult(parseResult).right
      .map((in: ((Int, Int), List[(RoverPositionOrientation, List[Command])])) => {
        val rovers = in._2
        val longestList = longestCommandListLength(rovers)
        val (positionsOrientations, commands) = rovers.unzip
        val paddedCommands: List[List[Command]] = commands.map(li => li.padTo(longestList, Noop))
        RoversInput(Rovers(in._1, positionsOrientations, List()), paddedCommands)
      })
    parsedInput
  }

  private def doParse(input: String): Either[String, ParserState] = {
    @tailrec
    def go(expectedLine: ParserState): Either[String, ParserState] = {
      val result: Either[String, ParserState] = expectedLine.process()

      //So nasty =_( But is there an easy way (no trampolining) to make this FP-tailrecursive?
      if (result.isLeft || !result.right.get.hasMore()) {
        result
      } else {
        go(result.right.get)
      }
    }

    go(PlateauDefinition(input.split("\n").toList))
  }

  private def transformResult(result: Either[String, ParserState]):
                    Either[String, ((Int, Int), List[(RoverPositionOrientation, List[Command])])] = {
    result match {
      case Right(RoverDefinitionOrEnd(_, dimensions, rovers)) => Right((dimensions, rovers.reverse))
      case Right(state) => Left(s"Parser error, incorrect state: $state")
      case Left(error: String) => Left(error)
    }
  }
}