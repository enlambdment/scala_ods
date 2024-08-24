package impl

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.Random

/**
 * RandomQueue implements the Queue interface such that the `dequeue()`
 * operation removes a random element selected from among those present
 * in the queue (rather than employing the FIFO order of a standard queue.)
 * Both operations mutating the RandomQueue run in constant amortized time
 * (where the amortization accounts for the cost of resizing the array.)
 *
 * @tparam A Type of the queue contents (items contained in the queue.)
 */
class RandomQueue[A: ClassTag] extends api.Queue[A] {
  private var n: Int = 0
  private var a: Array[A] = new Array[A](1)
  override def size(): Int = n

  private def resize(): Unit = {
    val s: Int = Math.max(2 * n, 1)
    val b: Array[A] = new Array[A](s)
    Array.copy(a, 0, b, 0, n)
    a = b
  }
  override def enqueue(x: A): Unit = {
    if (n + 1 > a.length) {
      resize()
    }
    a(n) = x
    n += 1
  }

  override def dequeue(): A = {
    if (n <= 0) {
      throw new IndexOutOfBoundsException("RandomQueue is empty!")
    }
    // select a random item to remove
    val i = Random.between(0, n)
    val y: A = a(i)
    a(i) = a(n - 1) // fill the "hole" due to removing y with the final array element
    n -= 1
    if (3 * n <= a.length) {
      resize()
    }
    y
  }

  /**
   * Overrides the conversion to Scala mutable.Queue[A] because that one
   * relies on the concrete implementation adding / removing elements in
   * FIFO order, which RandomQueue does not.
   *
   * @return The RandomQueue, converted to a standard scala Queue (with FIFO
   *         queuing discipline.)
   */
  override def toScalaQueue: mutable.Queue[A] = {
    mutable.Queue[A](a.take(n).toIndexedSeq:_*)
  }

  /**
   * At its heart, RandomQueue is really a multiset (or "bag") equipped
   * with add and (random) remove operations. An arguably better fit for
   * representing this using a data structure from the Scala collections
   * library would be a mutable map which associates each element with its
   * count (or frequency) of instances in the multiset.
   *
   * @return  A representation of the RandomQueue's contents as a "multiset",
   *          where each element is present as a key, and associated with a
   *          value counting its instances in the multiset.
   */
  def toScalaMap: mutable.HashMap[A, Int] = {
    val counts = new mutable.HashMap[A, Int]()
    for (i <- Range(0, n)) {
      val x: A = a(i)
      val count: Int = counts.getOrElse(x, 0)
      counts.update(x, count + 1)
    }
    counts
  }
}
