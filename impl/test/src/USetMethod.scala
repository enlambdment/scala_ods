package impl

object USetMethod {
  sealed trait USetMethod[A]
  case class USetSize[A]() extends USetMethod[A]
  case class USetAdd[A](x: A) extends USetMethod[A]
  case class USetRemove[A](x: A) extends USetMethod[A]
  case class USetFind[A](x: A) extends USetMethod[A]
}
