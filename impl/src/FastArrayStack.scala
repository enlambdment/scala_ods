package impl

import scala.reflect.ClassTag

class FastArrayStack[A: ClassTag] extends api.List[A] with api.Stack[A] {
  private var n: Int = 0
  private var a: Array[A] = new Array[A](1)

  /**
   * In order for custom instance implementation of `addAll` to work,
   * we need to avoid overflowing the backing Array (by resizing if
   * there is currently not enough room for the sequence being inserted.)
   *
   * By default, this behaves as for other `List` implementations which leverage
   * resizing, i.e. by doubling the size of the backing Array when there is not
   * enough room.
   *
   * @param newSize New size for the backing array.
   */
  private def resize(newSize: Int = Math.max(2 * n, 1)): Unit = {
    val b = new Array[A](newSize)
    Array.copy(a, 0, b, 0, n)
    a = b
  }

  override def size(): Int = n

  override def get(i: Int): A = {
    if (i < 0 || i > n - 1) {
      throw new IndexOutOfBoundsException()
    } else {
      a(i)
    }
  }

  override def set(i: Int, x: A): A = {
    if (i < 0 || i > n - 1) {
      throw new IndexOutOfBoundsException()
    } else {
      val y = a(i)
      a(i) = x
      y
    }
  }

  override def add(i: Int, x: A): Unit = {
    if (i < 0 || i > n) {
      throw new IndexOutOfBoundsException()
    }
    if (n + 1 > a.length) {
      resize()
    }
    Array.copy(a, i, a, i + 1, n - i)
    a(i) = x
    n += 1
  }

  override def remove(i: Int): A = {
    if (i < 0 || i > n - 1) {
      throw new IndexOutOfBoundsException()
    }
    val y = a(i)
    Array.copy(a, i + 1, a, i, n - i - 1)
    n -= 1
    if (3 * n <= a.length) {
      resize()
    }
    y
  }

  /**
   * Optimized implementation which avoids repeated shifting of the
   * array tail.
   *
   * By default, worst case running time is
   *  O(this.size * c.size)
   * which arises when inserting at i = 0.
   *
   * Overriding implementation avoids this by performing a single
   * subarray shift to make room for the inserted sequence, then
   * iterates over the sequence with a series of `set` calls.
   * Done this way, worst case running time is
   *  O(this.size + c.size)
   * again arising when inserting at i = 0.
   *
   * @param i Index for inserting a sequence of items at
   * @param c A sequence of items to be inserted at the index
   */
  override def addAll(i: Int, c: Iterable[A]): Unit = {
    val k = c.size
    if (n + k > a.length) {
      resize(2 * (n + k))
    }
    Array.copy(a, i, a, i + k, n - i)
    n += k
    var j = i
    for (x <- c) {
      this.set(j, x)
      j += 1
    }
  }

  // FastArrayStack efficiently implements the Stack operations in constant time
  override def push(x: A): Unit = {
    add(size(), x)
  }

  override def pop(): A = {
    remove(size())
  }
}
