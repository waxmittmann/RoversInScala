package me.max.marscontrol.entity.rover

import me.max.marscontrol.entity.Command
import me.max.marscontrol.util.RightBiasedEither.RightBiasedEither //This import makes the execute for work, DON'T DELETE!

object RoversAccumulator {
  def apply(rovers: Rovers): RoversAccumulator = RoversAccumulator(Right(rovers, List()))
  def apply(rovers: RoversInput): RoversAccumulator = RoversAccumulator(rovers.rovers).executeAll(rovers.commands)
}

case class RoversAccumulator(state: Either[(RoverError, List[Rovers]), (Rovers, List[Rovers])]) {
//  def executeAll(commands: List[List[Command]]): RoversAccumulator = executeAllHelper(commands.reverse)

//  protected def executeAllHelper(commands: List[List[Command]]): RoversAccumulator = {
  def executeAll(commands: List[List[Command]]): RoversAccumulator = { //executeAllHelper(commands.reverse)
    if (commands.head.size == 0) {
      this
    } else {
      //      val init = (List[Command](), List[List[Command]]())
      val init = (List[Command](), List[List[Command]]())
      val headAndTailCommands =  commands.foldRight(init)(
        //      val headAndTailCommands =  commands.foldLeft(init)(
        (cur, li) => ((cur.head :: li._1), (cur.tail :: li._2)))
      //        (li: (List[Command], List[List[Command]]), cur: List[Command]) => ((cur.head :: li._1), (cur.tail :: li._2)))
      val (headCommands, tailCommands) = (headAndTailCommands._1, headAndTailCommands._2)
//      execute(headCommands).executeAllHelper(tailCommands)
      println("At head " + headCommands)
      execute(headCommands).executeAll(tailCommands)
    }
  }

  def execute(command: List[Command]): RoversAccumulator = {
    val nextRoverState = for {
      roversAndHistory <- state.right
      rovers = roversAndHistory._1
      roverStateHistory = roversAndHistory._2
      newRoverState <- rovers.execute(command)
        .left.map(error => (error, roverStateHistory))
        .right
    } yield {
      (newRoverState, roverStateHistory :+ newRoverState)
//      (newRoverState, newRoverState :: roverStateHistory)
    }
    RoversAccumulator(nextRoverState)
  }

  override def toString: String = {
    def historyToString(history: List[Rovers]): String = {
      history.foldRight((1, ""))((rovers, str: (Int, String)) => {
        ((str._1 + 1), s"${str._2}\n#${str._1}: ${rovers.toString()}")
      })._2
    }

    state.fold(
      rovers => s"States leading up to error:\n${historyToString(rovers._2)}\nError:\n${rovers._1}",
      rovers => s"State history:\n${historyToString(rovers._2)}\nFinal State:\n${rovers._1}")
  }
}
