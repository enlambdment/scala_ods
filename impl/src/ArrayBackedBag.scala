package impl

import cats.kernel.Eq
import scala.reflect.ClassTag

import api.{PartitionedBag, Partition}

/**
 * A simple ArrayUSet-backed implementation of a Bag (multiset.) Not meant to be efficient.
 *
 * @param classTag$A$0 Type of values contained in the bag.
 * @param eqA There must be an instance of Eq[A].
 * @tparam A Type of contents of the unordered bag (multiset.)
 */
class ArrayBackedBag[A: ClassTag](
  implicit eqA: Eq[A]
) extends PartitionedBag[A, RootishArrayStack[A], ArrayUSet[Partition[A, RootishArrayStack[A]]]] {
  override val eq: Eq[A] = eqA
  override var items: ArrayUSet[Partition[A, RootishArrayStack[A]]] = new ArrayUSet()
  override val listFactory: RootishArrayStack[A] = new RootishArrayStack()
  override val iterator: Iterator[A] = for {
    partition <- items.iterator
    ms = partition.members
    m <- ms.toScala
  } yield { m }
}