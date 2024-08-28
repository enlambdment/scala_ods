package impl

import scala.reflect.ClassTag

/*
Efficient implementation of a List (hence, in particular, of a Deque)
by using two ArrayStacks. Achieves same performance bounds as the
ArrayDeque implementation (which relies on circular array technique.)
 */
class DualArrayDeque[A: ClassTag] extends api.List[A] with api.Deque[A] {
  /*
  Since the total collection size (number of elements) is always
  the sum of front.size() + back.size(), no need to track this
  as a separate private var
   */
  private var front: api.List[A] = new ArrayStack[A]
  private var back: api.List[A] = new ArrayStack[A]

  override def newInstance: DualArrayDeque[A] = new DualArrayDeque[A]()

  private def balance(): Unit = {
    val n: Int = size()
    if (n >= 2) {
      if (3 * front.size() < back.size()) { // case of front too small
        val s: Int = (n / 2) - front.size()
        val l1: api.List[A] = new ArrayStack[A]
        val l2: api.List[A] = new ArrayStack[A]
        // copy the first s many items from back into the new front, in reverse order
        for (j <- Range.inclusive(s - 1, 0, -1)) {
          l1.add(l1.size(), back.get(j))
        }
        // copy the items from existing front into the new front, in original order
        for (j <- Range(0, front.size())) {
          l1.add(l1.size(), front.get(j))
        }
        // copy the remaining items from back into the new back, in original order
        for (j <- Range(s, back.size())) {
          l2.add(l2.size(), back.get(j))
        }
        front = l1
        back = l2
      } else if (3 * back.size() < front.size()) { // case of back too small
        val s: Int = front.size() - (n / 2)
        val l1: api.List[A] = new ArrayStack[A]
        val l2: api.List[A] = new ArrayStack[A]
        for (j <- Range(s, front.size())) {
          l1.add(l1.size(), front.get(j))
        }
        for (j <- Range.inclusive(s - 1, 0, -1)) {
          l2.add(l2.size(), front.get(j))
        }
        for (j <- Range(0, back.size())) {
          l2.add(l2.size(), back.get(j))
        }
        front = l1
        back = l2
      }
    }
  }

  override def size(): Int = {
    front.size() + back.size()
  }

  override def get(i: Int): A = {
    if (i < 0 || i > size() - 1) {
      throw new IndexOutOfBoundsException()
    }
    if (i < front.size()) {
      front.get(front.size() - i - 1)
    } else {
      back.get(i - front.size())
    }
  }

  override def set(i: Int, x: A): A = {
    if (i < 0 || i > size() - 1) {
      throw new IndexOutOfBoundsException()
    }
    if (i < front.size()) {
      front.set(front.size() - i - 1, x)
    } else {
      back.set(i - front.size(), x)
    }
  }

  override def add(i: Int, x: A): Unit = {
    if (i < 0 || i > size()) {
      throw new IndexOutOfBoundsException()
    }
    if (i < front.size()) {
      front.add(front.size() - i, x)
    } else {
      back.add(i - front.size(), x)
    }
    balance()
  }

  override def remove(i: Int): A = {
    if (i < 0 || i > size() - 1) {
      throw new IndexOutOfBoundsException()
    }
    val x: A = if (i < front.size()) {
      front.remove(front.size() - i - 1)
    } else {
      back.remove(i - front.size())
    }
    balance()
    x
  }

  override def addFirst(x: A): Unit = add(0, x)

  override def removeFirst(): A = remove(0)

  override def addLast(x: A): Unit = add(size(), x)

  override def removeLast(): A = remove(size() - 1)
}
