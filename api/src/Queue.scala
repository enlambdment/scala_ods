package api

import scala.collection.mutable

trait Queue[A] {
  /* Not a part of the Open Data Structures Queue interface.
  * Need it for `toScalaQueue`.*/
  def size(): Int

  def enqueue(x: A): Unit
  def dequeue(): A

  def toScala: mutable.Queue[A] = {
    val xs = new mutable.Queue[A]()
    for (_ <- Range(0, this.size())) {
      val x = this.dequeue()
      xs.enqueue(x)
      this.enqueue(x)
    }
    xs
  }
}