package api

trait Deque[A] {
  def addFirst(x: A): Unit
  def removeFirst(): A
  def addLast(x: A): Unit
  def removeLast(): A
}