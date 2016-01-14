package me.max.marscontrol.entity.rover

import me.max.marscontrol.entity.Command

object RoversAccumulator {
  def apply(rovers: Rovers): RoversAccumulator = RoversAccumulator(Right(rovers, List()))
}

case class RoversAccumulator(state: Either[(RoverError, List[Rovers]), (Rovers, List[Rovers])]) {
  def execute(command: List[Command]): RoversAccumulator = {
    state.fold((_) => this, cur => RoversAccumulator({
      cur._1.execute(command).fold[Either[(RoverError, List[Rovers]), (Rovers, List[Rovers])]](
        error => Left((error, cur._2)),
        result => Right((result, result :: cur._2)))
    }))
  }
}

