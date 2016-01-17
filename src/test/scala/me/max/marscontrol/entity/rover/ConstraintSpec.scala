package me.max.marscontrol.entity.rover

import me.max.marscontrol.entity.Orientation.{South, East}
import org.specs2.mutable

class ConstraintSpec extends mutable.Specification {
  "Atomic Constraint" should {
    "should not fail when it evaluates to true" in {
      //Given
      val constraint = AtomicConstraint((rovers, positionOrientation) => {
        if (positionOrientation.orientation == East) {
          Right()
        } else {
          Left("Failed")
        }
      })

      val rovers = Rovers((1, 1), List(RoverPositionOrientation(0, 0, East)))

      //When
      val result = constraint.testRover(rovers, RoverPositionOrientation(0, 0, East))

      //Then
      result.isRight must beTrue
    }

    "should fail when it evaluates to false" in {
      //Given
      val constraint = AtomicConstraint((rovers, positionOrientation) => {
        if (positionOrientation.orientation == South) {
          Right()
        } else {
          Left("Failed")
        }
      })

      val rovers = Rovers((1, 1), List(RoverPositionOrientation(0, 0, East)))

      //When
      val result = constraint.testRover(rovers, RoverPositionOrientation(0, 0, East))

      //Then
      result.isLeft must beTrue
      result.left.get must beEqualTo("Failed")
    }
  }

  "Or Constraint" should {
    "fail when both subconstraints fail" in {
      //Given
      val atomicFailingConstraint = AtomicConstraint((rovers, positionOrientation) => {
        Left("Failed")
      })
      val compoundConstraint = atomicFailingConstraint or atomicFailingConstraint

      val rovers = Rovers((1, 1), List(RoverPositionOrientation(0, 0, East)))

      //When
      val result = compoundConstraint.testRover(rovers, RoverPositionOrientation(0, 0, East))

      //Then
      result.isLeft must beTrue
      result.left.get must beEqualTo("Not Failed  and not Failed")
    }

    "succeed when the first subconstraint succeeds" in {
      //Given
      val atomicSucceedingConstraint = AtomicConstraint((rovers, positionOrientation) => {
        Right()
      })
      val atomicFailingConstraint = AtomicConstraint((rovers, positionOrientation) => {
        Left("Failed")
      })

      val compoundConstraint = atomicSucceedingConstraint or atomicFailingConstraint

      val rovers = Rovers((1, 1), List(RoverPositionOrientation(0, 0, East)))

      //When
      val result = compoundConstraint.testRover(rovers, RoverPositionOrientation(0, 0, East))

      //Then
      result.isRight must beTrue
    }

    "succeed when the first subconstraint fails but the second subconstraint succeeds" in {
      //Given
      val atomicSucceedingConstraint = AtomicConstraint((rovers, positionOrientation) => {
        Right()
      })
      val atomicFailingConstraint = AtomicConstraint((rovers, positionOrientation) => {
        Left("Failed")
      })

      val compoundConstraint = atomicFailingConstraint or atomicSucceedingConstraint

      val rovers = Rovers((1, 1), List(RoverPositionOrientation(0, 0, East)))

      //When
      val result = compoundConstraint.testRover(rovers, RoverPositionOrientation(0, 0, East))

      //Then
      result.isRight must beTrue
    }

    "succeed when both subconstraints succeed" in {
      //Given
      val atomicSucceedingConstraint = AtomicConstraint((rovers, positionOrientation) => {
        Right()
      })

      val compoundConstraint = atomicSucceedingConstraint or atomicSucceedingConstraint

      val rovers = Rovers((1, 1), List(RoverPositionOrientation(0, 0, East)))

      //When
      val result = compoundConstraint.testRover(rovers, RoverPositionOrientation(0, 0, East))

      //Then
      result.isRight must beTrue
    }
  }

  "If Constraint" should {
    "call ifTrue constraint if condition constraint evaluates to true" in {
      //Given
      val atomicFailingConstraint = AtomicConstraint((rovers, positionOrientation) => {
        Right()
      })
      val ifTrueConstraint = AtomicConstraint((rovers, positionOrientation) => {
        Left("IfTrue")
      })
      val ifFalseConstraint = AtomicConstraint((rovers, positionOrientation) => {
        Left("IfFalse")
      })
      val ifConstraint = atomicFailingConstraint.ifc(ifTrueConstraint, ifFalseConstraint)
      val rovers = Rovers((1, 1), List(RoverPositionOrientation(0, 0, East)))

      //When
      val result = ifConstraint.testRover(rovers, RoverPositionOrientation(0, 0, East))

      //Then
      result.isLeft must beTrue
      result.left.get must beEqualTo("IfTrue")
    }

    "call ifFalse constraint if condition constraint evaluates to false" in {
      //Given
      val atomicFailingConstraint = AtomicConstraint((rovers, positionOrientation) => {
        Left("Failed")
      })
      val ifTrueConstraint = AtomicConstraint((rovers, positionOrientation) => {
        Left("IfTrue")
      })
      val ifFalseConstraint = AtomicConstraint((rovers, positionOrientation) => {
        Left("IfFalse")
      })
      val ifConstraint = atomicFailingConstraint.ifc(ifTrueConstraint, ifFalseConstraint)
      val rovers = Rovers((1, 1), List(RoverPositionOrientation(0, 0, East)))

      //When
      val result = ifConstraint.testRover(rovers, RoverPositionOrientation(0, 0, East))

      //Then
      result.isLeft must beTrue
      result.left.get must beEqualTo("IfFalse")
    }
  }

  "And Constraint" should {
    "fail when both subconstraints fail" in {
      //Given
      val atomicFailingConstraint = AtomicConstraint((rovers, positionOrientation) => {
        Left("Failed")
      })
      val compoundConstraint = atomicFailingConstraint and atomicFailingConstraint

      val rovers = Rovers((1, 1), List(RoverPositionOrientation(0, 0, East)))

      //When
      val result = compoundConstraint.testRover(rovers, RoverPositionOrientation(0, 0, East))

      //Then
      result.isLeft must beTrue
      result.left.get must beEqualTo("Failed")
    }

    "fail when the first subconstraint succeeds but the second fails" in {
      //Given
      val atomicSucceedingConstraint = AtomicConstraint((rovers, positionOrientation) => {
        Right()
      })
      val atomicFailingConstraint = AtomicConstraint((rovers, positionOrientation) => {
        Left("Failed")
      })

      val compoundConstraint = atomicSucceedingConstraint and atomicFailingConstraint

      val rovers = Rovers((1, 1), List(RoverPositionOrientation(0, 0, East)))

      //When
      val result = compoundConstraint.testRover(rovers, RoverPositionOrientation(0, 0, East))

      //Then
      result.isLeft must beTrue
      result.left.get must beEqualTo("Failed")
    }

    "fail when the first subconstraint fails but the second subconstraint succeeds" in {
      //Given
      val atomicSucceedingConstraint = AtomicConstraint((rovers, positionOrientation) => {
        Right()
      })
      val atomicFailingConstraint = AtomicConstraint((rovers, positionOrientation) => {
        Left("Failed")
      })

      val compoundConstraint = atomicFailingConstraint and atomicSucceedingConstraint

      val rovers = Rovers((1, 1), List(RoverPositionOrientation(0, 0, East)))

      //When
      val result = compoundConstraint.testRover(rovers, RoverPositionOrientation(0, 0, East))

      //Then
      result.isLeft must beTrue
      result.left.get must beEqualTo("Failed")
    }

    "succeed when both subconstraints succeed" in {
      //Given
      val atomicSucceedingConstraint = AtomicConstraint((rovers, positionOrientation) => {
        Right()
      })

      val compoundConstraint = atomicSucceedingConstraint and atomicSucceedingConstraint

      val rovers = Rovers((1, 1), List(RoverPositionOrientation(0, 0, East)))

      //When
      val result = compoundConstraint.testRover(rovers, RoverPositionOrientation(0, 0, East))

      //Then
      result.isRight must beTrue
    }
  }
}