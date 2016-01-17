package me.max.marscontrol.entity.rover

trait Constraint {
  def test(rovers: Rovers): Either[String, Unit]
}

case object StayWithinPlateau extends Constraint {
  override def test(rovers: Rovers): Either[String, Unit] = {
    val roverOutsideBounds = rovers.rovers.find((pos) => {
//      println("Testing " + pos)
//      println(s"((${pos.position.x} < ${rovers.plateau._1}) && (${pos.position.x} > 0) && (${pos.position.y} < ${rovers.plateau._2}) && (${pos.position.y > 0}))")
      ((pos.position.x >= rovers.plateau._1) || (pos.position.x < 0) ||
        (pos.position.y >= rovers.plateau._2) || (pos.position.y < 0))
    })

//    ((pos.position.x < rovers.plateau._1) || (pos.position.x > 0) ||
//        (pos.position.y < rovers.plateau._1) || (pos.position.y > 0)))

//    println("Checked, got " + roverOutsideBounds)
    roverOutsideBounds.fold[Either[String, Unit]](Right())(pos => Left(s"$pos out of bounds"))
  }
}
