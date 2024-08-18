package impl

object QueueMethod {
  sealed trait QueueMethod[A]
  case class QueueEnqueue[A](x: A) extends QueueMethod[A]
  case class QueueDequeue[A]() extends QueueMethod[A]
}
