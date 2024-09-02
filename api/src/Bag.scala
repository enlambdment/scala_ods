package api

import cats.kernel.Eq

trait Bag[A] extends USet[A] {
  implicit val eq: Eq[A]

  def findAll(x: A): List[A]
}
