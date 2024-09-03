package api

import cats.kernel.Eq
import scala.collection.mutable

trait Bag[A] {
  implicit val eq: Eq[A]

  def iterator: Iterator[A]
  def size(): Int
  def add(x: A): Boolean
  def remove(x: A): Option[A]
  def find(x: A): Option[A]
  def findAll(x: A): List[A]

  /*
  The Scala collections standard library doesn't really have
  a data structure for multisets. We "fake it" as a Map[A, List[A]].
   */
  def toScala: mutable.HashMap[A, mutable.ListBuffer[A]]
}
