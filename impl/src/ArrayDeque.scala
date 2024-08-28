package impl

import scala.reflect.ClassTag

class ArrayDeque[A: ClassTag] extends api.List[A] with api.Deque[A] {
  private var a = new Array[A](1)
  private var j = 0
  private var n = 0

  override def newInstance: ArrayDeque[A] = new ArrayDeque[A]()

  private def resize(): Unit = {
    val b = new Array[A](Math.max(2 * n, 1))
    for (i <- Range(0, n)) {
      b(i) = a((j + i) % a.length)
    }
    a = b
    j = 0
  }

  override def size(): Int = n

  override def get(i: Int): A = {
    if (i < 0 || i > n - 1) {
      throw new IndexOutOfBoundsException()
    }
    a((j + i) % a.length)
  }

  override def set(i: Int, x: A): A = {
    if (i < 0 || i > n - 1) {
      throw new IndexOutOfBoundsException()
    }
    val k = (j + i) % a.length // calculate position just once
    val y = a(k)
    a(k) = x
    y
  }

  override def add(i: Int, x: A): Unit = {
    if (i < 0 || i > n) {
      throw new IndexOutOfBoundsException()
    }
    if (n + 1 > a.length) { resize() }
    if (2 * i < n) {
      for (k <- Range(0, i)) {
        /* We need to be able to cope with
        *   negative params to modulo operator;
        *   so use Math.floorMod not % */
        a(Math.floorMod(j + k - 1, a.length)) = a(Math.floorMod(j + k, a.length))
      }
      a(Math.floorMod(j + i - 1, a.length)) = x
      j = Math.floorMod(j - 1, a.length)
      n += 1
    } else {
      for (k <- Range(n - 1, i - 1, -1)) {
        a(Math.floorMod(j + k + 1, a.length)) = a(Math.floorMod(j + k, a.length))
      }
      a((j + i) % a.length) = x
      n += 1
    }
  }

  override def remove(i: Int): A = {
    if (i < 0 || i > n - 1) {
      throw new IndexOutOfBoundsException()
    }
    val x: A = a((j + i) % a.length)
    if (2 * i < n) {
      for (k <- Range(i, 0, -1)) {
        a((j + k) % a.length) = a((j + k - 1) % a.length)
      }
      j = (j + 1) % a.length
    } else {
      for (k <- Range(i, n - 1)) {
        a((j + k) % a.length) = a((j + k + 1) % a.length)
      }
    }
    n -= 1
    if (3 * n <= a.length) { resize() }
    x
  }

  // ArrayDeque efficiently implements the Deque operations in constant time
  override def addFirst(x: A): Unit = add(0, x)

  override def removeFirst(): A = remove(0)

  override def addLast(x: A): Unit = add(size(), x)

  override def removeLast(): A = remove(size() - 1)
}
