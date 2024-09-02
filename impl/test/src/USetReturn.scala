package impl

import cats.kernel.Eq

object USetReturn {
  sealed trait USetReturn[A]
  case class USetInt[A](n: Int) extends USetReturn[A]
  case class USetBoolean[A](b: Boolean) extends USetReturn[A]
  case class USetAOpt[A](aOpt: Option[A]) extends USetReturn[A]

  implicit def eqUSetReturn[A](implicit eqA: Eq[A]): Eq[USetReturn[A]] = new Eq[USetReturn[A]] {
    override def eqv(x: USetReturn[A], y: USetReturn[A]): Boolean = {
      (x, y) match {
        case (USetInt(m),     USetInt(n))     => m == n
        case (USetBoolean(b), USetBoolean(c)) => b == c
        case (USetAOpt(aOpt), USetAOpt(bOpt)) => Eq.eqv(aOpt, bOpt)
        case _                                => false
      }
    }
  }
}
