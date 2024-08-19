package impl

import impl.{ArrayStack => ODSStack}
import scala.reflect.ClassTag

class RootishArrayStack[A: ClassTag] extends api.List[A] with api.Stack[A] {
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

  override def size(): Int = ???

  override def get(i: Int): A = ???

  override def set(i: Int, x: A): A = ???

  override def add(i: Int, x: A): Unit = ???

  override def remove(i: Int): A = ???

  override def push(x: A): Unit = ???

  override def pop(): A = ???
}
