package impl

object ListMethod {
  sealed trait ListMethod[A]
  case class ListSize[A]() extends ListMethod[A]
  case class ListGet[A](i: Int) extends ListMethod[A]
  case class ListSet[A](i: Int, x: A) extends ListMethod[A]
  case class ListAdd[A](i: Int, x: A) extends ListMethod[A]
  case class ListRemove[A](i: Int) extends ListMethod[A]
}