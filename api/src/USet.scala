package api

import cats.kernel.Eq
import scala.collection.mutable

/**
 * "The USet interface represents an unordered set of unique
 * elements, which mimics a mathematical set. A USet contains
 * n _distinct_ elements; no elements appears more than once; the
 * elements are in no specific order."
 *
 * The method type signatures given in the Java companion code allow
 * for the possibility for null to be returned, if remove / find operations
 * fail to find a match. In Scala, Option[A] is more idiomatic (with
 * possibility of failure handled by returning None.)
 *
 * Also, I am choosing to leverage cats typeclass `Eq` for typesafe equality
 * (and so that I can make the requirement for a reasonable, type-specific
 * notion of equality explicit in the type bounds for A.)
 *
 * @tparam A  Type of contents of the unordered set.
 */
trait USet[A] {
  implicit val eq: Eq[A]

  /*
  Not a part of the ODS interface for USet, but needed for later on.
   */
  def iterator: Iterator[A]

  def size(): Int
  def add(x: A): Boolean
  def remove(x: A): Option[A]
  def find(x: A): Option[A]

  def toScala: mutable.HashSet[A]
}
