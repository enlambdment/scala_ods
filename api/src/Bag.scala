package api

import cats.kernel.Eq

/**
 * Using a USet, implement a Bag (a.k.a. a Multiset.)
 * A Bag is like a USet — it supports the add(x), remove(x) and find(x) methods —
 * but it allows duplicate elements to be stored.
 *
 * The find(x) operation in a Bag returns some element (if any) that is equal to x.
 * In addition, a Bag supports the findAll(x) operation that returns a list of all
 * elements in the Bag that are equal to x.
 *
 * @tparam A Type of contents of the unordered bag (multiset.)
 */
trait Bag[A, L <: List[A], B <: USet[Partition[A, L]]] extends USet[A] {
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
   * Default implementations for the bag methods, leveraging the methods
   * of the backing USet as well as the behavior due to custom Eq instance
   * for Partition[A].
   */
  override def size(): Int = items
    .iterator
    .map(_.members.size())
    .sum

  /**
   * Add the element x to the bag, regardless of whether equal elements are already present.
   *
   * @param x Item to be added to the bag.
   * @return  Boolean indicating whether (any element equal to) x was already present in the bag.
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
  def findAll(x: A): List[A] = {
    val xPartition: Partition[A, L] = Partition(x, listFactory.newInstance.asInstanceOf[L])
    val currentPartitionOpt: Option[Partition[A, L]] = items.find(xPartition).filter(
      _.hasItems
    )
    currentPartitionOpt.map(_.members).getOrElse(listFactory.newInstance.asInstanceOf[L])
  }
}
