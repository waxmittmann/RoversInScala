package me.max.marscontrol.entity.rover

import me.max.marscontrol.entity.Command
import me.max.marscontrol.util.RightBiasedEither.RightBiasedEither

object RoversAccumulator {
  def apply(rovers: Rovers): RoversAccumulator = RoversAccumulator(Right(rovers, List()))
}

case class RoversAccumulator(state: Either[(RoverError, List[Rovers]), (Rovers, List[Rovers])]) {
  def executeAll(commands: List[List[Command]]): RoversAccumulator = ???

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
