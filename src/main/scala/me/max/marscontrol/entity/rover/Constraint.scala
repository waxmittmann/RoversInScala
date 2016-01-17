package me.max.marscontrol.entity.rover

trait Constraint {
  def test(rovers: Rovers): Either[String, Unit]
}

case object StayWithinPlateau extends Constraint {
  override def test(rovers: Rovers): Either[String, Unit] = {
    val roverOutsideBounds = rovers.rovers.find((pos) => {
      ((pos.position.x >= rovers.plateau._1) || (pos.position.x < 0) ||
        (pos.position.y >= rovers.plateau._2) || (pos.position.y < 0))
    })

    roverOutsideBounds.fold[Either[String, Unit]](Right())(pos => Left(s"$pos out of bounds"))
  }
}
