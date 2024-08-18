package impl

import scala.reflect.ClassTag

class ArrayStack[A: ClassTag] extends api.List[A] with api.Stack[A] {
  private var n: Int = 0
  private var a: Array[A] = new Array[A](1)

  private def resize(): Unit = {
    val b = new Array[A](Math.max(2 * n, 1))
    for (i <- Range(0, n)) {
      b(i) = a(i)
    }
    a = b
  }

  override def size(): Int = n

  override def get(i: Int): A = {
    if (i < 0 || i > n - 1) {
      throw new IndexOutOfBoundsException()
    }
    a(i)
  }

  override def set(i: Int, x: A): A = {
    if (i < 0 || i > n - 1) {
      throw new IndexOutOfBoundsException()
    }
    val y = a(i)
    a(i) = x
    y
  }

  override def add(i: Int, x: A): Unit = {
    if (i < 0 || i > n) {
      throw new IndexOutOfBoundsException()
    }
    if (n + 1 > a.length) {
      resize()
    }
    for (j <- Range(n, i, -1)) {
      a(j) = a(j - 1)
    }
    a(i) = x
    n += 1
  }

  override def remove(i: Int): A = {
    if (i < 0 || i > n - 1) {
      throw new IndexOutOfBoundsException()
    }
    val x = a(i)
    for (j <- Range(i, n - 1)) {
      a(j) = a(j + 1)
    }
    n -= 1
    if (3 * n <= a.length) {
      resize()
    }
    x
  }

  // ArrayStack efficiently implements the Stack operations in constant time
  override def push(x: A): Unit = {
    add(size(), x)
  }

  override def pop(): A = {
    remove(size())
  }
}