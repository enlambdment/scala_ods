package api

import scala.collection.mutable

trait List[A] extends Stack[A] {
  def size(): Int
  def get(i: Int): A
  def set(i: Int, x: A): A
  def add(i: Int, x: A): Unit
  def remove(i: Int): A

  // Default implementation for Stack methods, available for any instance of List
  override def newInstance: List[A]
  override def push(x: A): Unit = {
    add(size(), x)
  }

  override def pop(): A = {
    remove(size() - 1)
  }

  /**
   * Default implementation of the addAll operation for a sequence `c` of items
   * to be inserted at an index `i`.
   * Implementation performs repeated `add` calls, which may be inefficient in general.
   * Concrete implementations of `List` may override for better performance.
   *
   * @param i Index for inserting a sequence of items at
   * @param c A sequence of items to be inserted at the index
   */
  def addAll(i: Int, c: Iterable[A]): Unit = {
    var idx: Int = i
    for (x <- c) {
      this.add(idx, x)
      idx += 1
    }
  }

  def toScalaListBuffer: mutable.ListBuffer[A] = {
    val xs = new mutable.ListBuffer[A]()
    for (i <- Range(0, this.size)) {
      xs.addOne(this.get(i))
    }
    xs
  }
}