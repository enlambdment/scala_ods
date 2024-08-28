package api

import cats.kernel.Eq

case class Partition[A, L <: List[A]](
  key: A,
  members: L
) {
  def hasNoItems: Boolean = members.size() == 0
  def hasItems: Boolean = !hasNoItems
}

object Partition {
  implicit def eqPartition[A, L <: List[A]](implicit eqKey: Eq[A]): Eq[Partition[A, L]] = new Eq[Partition[A, L]] {
    override def eqv(x: Partition[A, L], y: Partition[A, L]): Boolean = {
      eqKey.eqv(x.key, y.key)
    }
  }
}