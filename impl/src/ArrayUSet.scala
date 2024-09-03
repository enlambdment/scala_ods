package impl

import cats.kernel.Eq

import scala.reflect.ClassTag
import scala.collection.mutable

/**
 * A simple list-backed implementation of a USet (unordered set.) Merely for test-driving
 * so I know how it plays with the cats typeclass Eq (and can check that Bag does the right thing.)
 *
 * This is about as bad as it gets efficiency-wise: adding an arbitrary element
 * is O(n) worst-case in n := this.size(), because we have to check the entire list
 * to make sure no equal values are already present. And since we don't maintain a
 * sorted backing list, there's no way to do this without confirming that every single
 * value in the backing list doesn't equal the one we're trying to add.
 *
 * @param classTag$A$0  Type of values contained in the unordered set.
 * @param eqA There must be an instance of Eq[A] (i.e. the members of type A must be
 *            comparable in a defined sense) in order to build ArrayUSet's representing
 *            an unordered set of values in A
 * @tparam A  Type of contents of the unordered set.
 */
class ArrayUSet[A: ClassTag](implicit eqA: Eq[A]) extends api.USet[A] {
  override implicit val eq: Eq[A] = eqA

  private val xs: mutable.ListBuffer[A] = new mutable.ListBuffer[A]

  override def iterator: Iterator[A] = xs.iterator

  override def size(): Int = xs.length

  override def add(x: A): Boolean = {
    val notPresent = !xs.contains(x)
    if (notPresent) {
      xs.insert(xs.length, x)
    }
    notPresent
  }

  override def remove(x: A): Option[A] = {
    val foundAt: Int = xs.indexWhere(eq.eqv(x, _))
    val result: Option[A] = if (foundAt > -1) {
      Some(xs.remove(foundAt))
    } else {
      None
    }
    result
  }

  override def find(x: A): Option[A] = {
    val foundAt: Int = xs.indexWhere(eq.eqv(x, _))
    val result: Option[A] = if (foundAt > -1) {
      Some(xs(foundAt))
    } else {
      None
    }
    result
  }

  override def toScala: mutable.HashSet[A] = {
    val hashSet = new mutable.HashSet[A]()
    for (elem <- this.iterator) { hashSet.add(elem) }
    hashSet
  }
}
