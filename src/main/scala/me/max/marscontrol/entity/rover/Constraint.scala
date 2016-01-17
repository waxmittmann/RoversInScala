package me.max.marscontrol.entity.rover

case class AtomicConstraint(testF: (Rovers, RoverPositionOrientation) => Either[String, Unit]) extends Constraint {
  override def testRover(state: Rovers, rover: RoverPositionOrientation): Either[String, Unit] = testF(state, rover)
}

/*Would make sense to distinguish different types of constraints:
    - individual rover (e.g. whether the rover is on plateau or not)
    - between-rovers (e.g. collision)
  (though given the current implementation of rovers as a list, that would not product an asymptotic improvement in
    constraint evaluation, factoring out the rover-to-rover part would make for easier-to-write between-rovers conditions)
 */
trait Constraint { self =>
  def test(rovers: Rovers): Either[String, Unit] = {
    rovers.rovers.map((rover) => testRover(rovers, rover)).find(cur => cur.isLeft).getOrElse(Right())
  }

  def testRover(state: Rovers, rover: RoverPositionOrientation): Either[String, Unit]

  def and(constraint: Constraint): Constraint = {
    new Constraint {
      override def testRover(state: Rovers, rover: RoverPositionOrientation): Either[String, Unit] = {
        self.testRover(state, rover).right.flatMap(_ => constraint.testRover(state, rover))
      }
    }
  }

  def or(constraint: Constraint): Constraint = {
    new Constraint {
      override def testRover(state: Rovers, rover: RoverPositionOrientation): Either[String, Unit] = {
        self.testRover(state, rover).left.flatMap(lhsError =>
          constraint.testRover(state, rover).left.map(rhsError => s"Not $lhsError  and not $rhsError"))
      }
    }
  }

  def ifc(ifTrue: Constraint, ifFalse: String => Constraint): Constraint = {
    new Constraint {
      override def testRover(state: Rovers, rover: RoverPositionOrientation): Either[String, Unit] = {
        (self.testRover(state, rover).fold(
          error => ifFalse(error).testRover(state, rover),
          _ => ifTrue.testRover(state, rover)))
      }
    }
  }
}

object Constraints {
  val Succeeds = AtomicConstraint((_, _) => Right())
  def Fails(message: String) = AtomicConstraint((_, _) => Left(message))

  val StayWithinXMinimum = AtomicConstraint((_, rover) => if (rover.position.x >= 0) Right() else Left(s"$rover x-bound is < 0"))
  val StayWithinXMaximum = AtomicConstraint((state, rover) => if (rover.position.x < state.plateau._1) Right() else Left(s"$rover x-bound is > ${state.plateau._1}"))
  val StayWithinYMinimum = AtomicConstraint((_, rover) => if (rover.position.y >= 0) Right() else Left(s"$rover y-bound is < 0"))
  val StayWithinYMaximum = AtomicConstraint((state, rover) => if (rover.position.y < state.plateau._2) Right() else Left(s"$rover y-bound is > ${state.plateau._2}"))

  val StayWithinPlateau = StayWithinXMinimum and StayWithinXMaximum and StayWithinYMinimum and StayWithinYMaximum

  val StayWithinXOrWithinY = (StayWithinXMinimum and StayWithinXMaximum) or (StayWithinYMinimum and StayWithinYMaximum)
  
  val PreventCollision = AtomicConstraint((state, rover) => {
    val roversWithSamePosition = state.rovers.foldLeft(0)((count, cur) => if (cur.position == rover.position) count + 1 else count)
    if (roversWithSamePosition > 1) {
      Left(s"${roversWithSamePosition} rovers have same position ${rover.position}")
    } else {
      Right()
    }
  })
  
  //Silly, but it demonstrates the concept; check for collision only if rover is within bounds
  val CheckCollisionIfWithinPlateau = StayWithinPlateau.ifc(PreventCollision, Fails(_))
}
