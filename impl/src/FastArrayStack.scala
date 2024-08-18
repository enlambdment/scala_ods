package impl

import scala.reflect.ClassTag

class FastArrayStack[A: ClassTag] extends api.List[A] with api.Stack[A] {
  private var n: Int = 0
  private var a: Array[A] = new Array[A](1)

  private def resize(): Unit = {
    val b = new Array[A](Math.max(2 * n, 1))
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

  // FastArrayStack efficiently implements the Stack operations in constant time
  override def push(x: A): Unit = {
    add(size(), x)
  }

  override def pop(): A = {
    remove(size())
  }
}
