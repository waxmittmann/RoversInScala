package me.max.marscontrol.entity

case class Position(x: Int, y: Int) {
  def increment(xi: Int, yi: Int) = Position(x + xi, y + yi)
}
