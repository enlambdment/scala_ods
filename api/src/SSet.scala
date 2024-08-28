package api

import cats.kernel.Eq

/**
 * "The SSet interface represents a sorted set of elements. An SSet stores elements from
 * some total order, so that any two elements x and y can be compared."
 *
 * The size / add / remove methods have exactly the same semantics as they do for USet.
 * However, SSet find performs *successor search* - i.e. calling `find(x)` attempts to
 * locate the smallest element y in the set such that y >= x. For this reason, both an
 * Eq[A] and an Ordering[A] instance are necessary.
 *
 * @tparam A Type of contents of the ordered set.
 */
trait SSet[A] {
  implicit val eq: Eq[A]
  implicit val ordering: Ordering[A]
  def size(): Int
  def add(x: A): Boolean
  def remove(x: A): Option[A]
  def find(x: A): Option[A]
}
