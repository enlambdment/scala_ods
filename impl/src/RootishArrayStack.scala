package impl

import api.Stack
import impl.{ArrayStack => ODSStack}

import scala.reflect.ClassTag

class RootishArrayStack[A: ClassTag] extends api.List[A] {
  /*
  There are r backing arrays at any time, numbered with 0-indexing
  (0, 1, 2, ..., r - 1.) These are called _blocks_ and they are tracked
  in a sequential data structure, with each item of the sequence `blocks`
  intended as a "pointer" to a backing array of a certain size.

  Since, by implementation, backing array #j contains (j + 1) elements,
  the entire data structures has room for up to
    1 + 2 + 3 + ... + r = r * (r + 1) / 2
  elements at any time, where r = blocks.size.

  Since we are only ever going to be adding a new block at the end, or
  removing the largest block present if it goes unused, `blocks` can
  in principle be any concrete structure implementing the api.Stack interface.
  We will just use api.List to be more specific (and follow the exposition in the text.)
   */
  private val blocks: api.List[Array[A]] = new ODSStack[Array[A]]()
  /* Private counter for number of elements. */
  private var n: Int = 0

  override def newInstance: RootishArrayStack[A] = new RootishArrayStack[A]()

  /**
   * Given an index i into the list (based on zero-indexing),
   * get the block index into `blocks` where that item should
   * be stored (also based on zero-indexing.)
   * @param i Index of item in the list being stored.
   * @return  Index of the block containing the item at that index.
   */
  private def i2b(i: Int): Int = {
    val db: Double = (-3 + Math.sqrt(9 + (8 * i))) / 2.0
    Math.ceil(db).toInt
  }

  private def grow(): Unit = {
    val s = blocks.size()       // get current # of blocks, which = size of largest block
    val b = new Array[A](s + 1) // next block will contain 1 more item than the previous
    blocks.add(s, b)
  }

  private def shrink(): Unit = {
    // remove the topmost block, located at (s - 1) where s = # of blocks being used
    val s = blocks.size()
    if (s > 0) {
      blocks.remove(s - 1)
    }
  }

  override def size(): Int = n

  override def get(i: Int): A = {
    val b: Int = i2b(i)
    val j: Int = i - ((b * (b + 1)) / 2) // calculate offset along the block
    blocks.get(b)(j)
  }

  override def set(i: Int, x: A): A = {
    val b: Int = i2b(i)
    val j: Int = i - ((b * (b + 1)) / 2) // calculate offset along the block
    val y: A = blocks.get(b)(j)
    blocks.get(b)(j) = x
    y
  }

  override def add(i: Int, x: A): Unit = {
    val r = blocks.size()
    if (n == r * (r + 1) / 2) {
      grow()
    }
    for (j <- Range(n, i, -1)) {
      this.set(j, this.get(j - 1))
    }
    this.set(i, x)
    n += 1
  }

  override def remove(i: Int): A = {
    val x: A = this.get(i)
    for (j <- Range(i + 1, n)) {
      this.set(j - 1, this.get(j))
    }
    n -= 1
    val r = blocks.size()
    // once there are more than 1 blocks going unused, remove one
    if (n <= (r - 2) * (r - 1) / 2) {
      shrink()
    }
    x
  }
}
