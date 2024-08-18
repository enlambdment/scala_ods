package api

import scala.collection.mutable

trait List[A] {
  def size(): Int
  def get(i: Int): A
  def set(i: Int, x: A): A
  def add(i: Int, x: A): Unit
  def remove(i: Int): A

  def toScalaListBuffer: mutable.ListBuffer[A] = {
    val xs = new mutable.ListBuffer[A]()
    for (i <- Range(0, this.size)) {
      xs.addOne(this.get(i))
    }
    xs
  }
}