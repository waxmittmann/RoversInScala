package me.max.marscontrol.entity

import org.mockito.Matchers.{eq => mockitoEq, _}
import org.specs2._
import specification.{Before, Scope}
import org.specs2.mock.Mockito

class PositionSpec extends mutable.Specification with Mockito {
  "Position" should {
    "be updated correctly via increment" in {
      //Given
      val initialPos = Position(1, 2)

      //When
      val newPos = initialPos.increment(-2, 3)

      //Then
      newPos.x must beEqualTo(-1)
      newPos.y must beEqualTo(5)
    }
  }
}
