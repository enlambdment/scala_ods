package api

trait Stack[A] {
  def push(x: A): Unit
  def pop(): A
}