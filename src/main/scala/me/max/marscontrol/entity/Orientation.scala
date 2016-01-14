package me.max.marscontrol.entity

object Orientation {

   sealed trait Orientation

   case object North extends Orientation

   case object South extends Orientation

   case object West extends Orientation

   case object East extends Orientation

   def fromString(strRep: String): Option[Orientation] = {
     strRep match {
       case "N" => Some(North)
       case "W" => Some(West)
       case "E" => Some(East)
       case "S" => Some(South)
       case _ => None
     }
   }
 }
