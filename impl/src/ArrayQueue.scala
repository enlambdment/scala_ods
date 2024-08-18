package impl

import scala.reflect.ClassTag


// ./mill -i impl.console
class ArrayQueue[A: ClassTag] extends api.Queue[A] {
  private var a: Array[A] = new Array[A](1)
  private var n: Int = 0
  private var j: Int = 0

  private def resize(): Unit = {
    val b = new Array[A](Math.max(2 * n, 1))
    for (i <- Range(0, n)) {
      b(i) = a((j + i) % a.length)
    }
    a = b
    j = 0
  }

  /* not strictly needed */
  def size(): Int = n

  override def enqueue(x: A): Unit = {
    if (n + 1 > a.length) {
      resize()
    }
    a((j + n) % a.length) = x
    n += 1
  }

  override def dequeue(): A = {
    if (n == 0) {
      throw new NoSuchElementException()
    }
    val x: A = a(j)
    j = (j + 1) % a.length
    n -= 1
    if (3 * n <= a.length) {
      resize()
    }
    x
  }
}
