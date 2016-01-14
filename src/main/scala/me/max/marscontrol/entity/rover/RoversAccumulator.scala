package me.max.marscontrol.entity.rover

import me.max.marscontrol.entity.Command
import me.max.marscontrol.util.RightBiasedEither.RightBiasedEither

object RoversAccumulator {
  def apply(rovers: Rovers): RoversAccumulator = RoversAccumulator(Right(rovers, List()))
}

case class RoversAccumulator(state: Either[(RoverError, List[Rovers]), (Rovers, List[Rovers])]) {
  def executeAll(commands: List[List[Command]]): RoversAccumulator = {
    if (commands.head.size == 0) {
      this
    } else {
//      execute(commands.head).executeAll(commands.tail)

      //Do this in one?
      val headCommands = commands.foldRight(List[Command]())((cur, li) => cur.head :: li)
      val tailCommands = commands.foldRight(List[List[Command]]())((
                                cur: List[Command], li: List[List[Command]]) => cur.tail :: li)

      execute(headCommands).executeAll(tailCommands)
    }
  }

  def execute(command: List[Command]): RoversAccumulator = {
    RoversAccumulator(for {
      roversAndHistory <- state.right
      rovers = roversAndHistory._1
      roverStateHistory = roversAndHistory._2
      newRoverState <- rovers.execute(command)
        .left.map(error => (error, roverStateHistory))
        .right
    } yield {
      (newRoverState, newRoverState :: roverStateHistory)
    })
  }
}
