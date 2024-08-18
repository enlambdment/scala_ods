package api

trait Queue[A] {
  def enqueue(x: A): Unit
  def dequeue(): A
}