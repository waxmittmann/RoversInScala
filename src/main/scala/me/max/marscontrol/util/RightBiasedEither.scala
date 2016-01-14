package me.max.marscontrol.util

//Got this from 'https://issues.scala-lang.org/browse/SI-5793'
//Not sure it's wise, but it makes the RoversAccumulator for-comprehension much prettier!
object RightBiasedEither {
  implicit class RightBiasedEither[A, B](val e: Either[A, B]) extends AnyVal {
    def foreach[U](f: B => U): Unit = e.right.foreach(f)

    def map[C](f: B => C): Either[A, C] = e.right.map(f)

    def flatMap[C](f: B => Either[A, C]) = e.right.flatMap(f)
  }
}
