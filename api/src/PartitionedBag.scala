package api

import cats.kernel.Eq

/**
 * The abstract trait PartitionedBag captures the pattern whereby a Bag is implemented
 * as an unordered set of partitions. Each partition stores an equivalence class of items
 * from the bag, where the equivalence is due to the `Eq[A]` instance provided.
 *
 * There are two choices to be made when using PartitionedBag in order to yield a
 * concrete implementation of Bag: the List implementation, and the USet (unordered set)
 * implementation.
 *
 * The choice of `L <: List[A]` gives the backing structure for storing items in the Bag
 * belonging to a given equivalence class, while the choice of `B <: USet[Partition[A, L]]`
 * gives the backing structure for tracking distinct partitions, in a (non-multi-)set.
 *
 * @tparam A  Type of contents of the unordered bag (multiset.)
 * @tparam L  List implementation used by Partition to store each equivalence class
 *            of elements, which (since this is a multiset) may contain multiple items.
 * @tparam B  USet implementation used for storing an unordered set of partitions:
 *            this is the backing structure which implements the multiset.
 */
trait PartitionedBag[A, L <: List[A], B <: USet[Partition[A, L]]] extends Bag[A] {
  implicit val eq: Eq[A]

  /**
   * The backing structure for the bag is to be an unordered set of
   * item partitions. Each partition is, underneath, a key representing
   * some "delegate" from the equivalence class due to `Eq[A]`, and its associated
   * list of members, from `A`. By using a list, we can store multiple members of
   * each equivalence class, i.e. "multiple copies" of an element which are otherwise
   * equivalent. Any implementation of USet works here.
   */
  var items: B
  val listFactory: L

  /**
   * Default implementations for the USet methods, in the specific context of
   * a multiset. Leverages the methods of the backing USet as well as the behavior
   * due to custom Eq instance for Partition[A].
   */
  override def size(): Int = items
    .iterator
    .map(_.members.size())
    .sum

  /**
   * Add the element x to the bag, regardless of whether equal elements are already present.
   *
   * @param x Item to be added to the bag.
   * @return  Boolean indicating whether x is a new addition to the bag (i.e. there were no
   *          existing elements equal to x.)
   */
  override def add(x: A): Boolean = {
    val emptyPartition: Partition[A, L] = Partition(x, listFactory.newInstance.asInstanceOf[L])
    val currentPartitionOpt: Option[Partition[A, L]] = items.remove(emptyPartition)
    val result: Boolean = currentPartitionOpt.isEmpty
    val xPartition = currentPartitionOpt.getOrElse(emptyPartition)
    xPartition.members.push(x)
    items.add(xPartition)
    result
  }

  /**
   * Remove (an element equal to) x from the bag, if it exists.
   *
   * @param x An item to be removed from the bag.
   * @return  An element matching x, if one exists (formally, this is an element belonging
   *          to the equivalence class of x due to `Eq[A]`, and present in the bag.)
   */
  override def remove(x: A): Option[A] = {
    val xPartition: Partition[A, L] = Partition(x, listFactory.newInstance.asInstanceOf[L])
    val currentPartitionOpt: Option[Partition[A, L]] = items.remove(xPartition).filter(
      _.hasItems
    )
    val removedOpt: Option[A] = currentPartitionOpt.map {
      _.members.pop()
    }
    // Only put back the Partition representing x if it still has items.
    if (currentPartitionOpt.exists(_.hasItems)) {
      currentPartitionOpt.foreach(items.add)
    }
    removedOpt
  }

  /**
   * Find (an element equal to) x from the bag, if it exists.
   *
   * @param x An item to be searched in the bag.
   * @return An element matching x, if one exists (formally, this is an element belonging
   *         to the equivalence class of x due to `Eq[A]`, and present in the bag.)
   */
  override def find(x: A): Option[A] = {
    val xPartition: Partition[A, L] = Partition(x, listFactory.newInstance.asInstanceOf[L])
    val currentPartitionOpt: Option[Partition[A, L]] = items.find(xPartition).filter(
      _.hasItems
    )
    val matchOpt: Option[A] = currentPartitionOpt.map { p =>
      val xMembers: List[A] = p.members
      xMembers.get(xMembers.size() - 1)
    }
    matchOpt
  }

  /**
   * Return a list of all elements from the bag that are equal to x.
   *
   * @param x An item to be searched in the bag.
   * @return All elements from the bag that are equal to x
   */
  override def findAll(x: A): List[A] = {
    val xPartition: Partition[A, L] = Partition(x, listFactory.newInstance.asInstanceOf[L])
    val currentPartitionOpt: Option[Partition[A, L]] = items.find(xPartition).filter(
      _.hasItems
    )
    currentPartitionOpt.map(_.members).getOrElse(listFactory.newInstance.asInstanceOf[L])
  }
}
