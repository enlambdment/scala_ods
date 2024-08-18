package impl

import impl.{QueueMethod => QM}
import org.scalacheck.{Arbitrary, Gen}

import scala.util.Try

object QueueTestUtils {
  def genQEnqueue[A](implicit arb: Arbitrary[A]): Gen[QM.QueueMethod[A]] = for {
    x <- Arbitrary.arbitrary[A]
  } yield QM.QueueEnqueue(x)

  def genQDequeue[A]: Gen[QM.QueueMethod[A]] = Gen.const(QM.QueueDequeue[A]())

  def genQActionAndNextSize[A](s: Int)(implicit arb: Arbitrary[A]): Gen[(QM.QueueMethod[A], Int)] =
    if (s > 0) {
      Gen.frequency(
        (2, genQEnqueue[A].map(qm => (qm, s))),
        (1, genQDequeue[A].map(qm => (qm, s)))
      )
    } else {
      genQEnqueue[A].map(qm => (qm, s))
    }

  def genQActions[A](s0: Int, nActions: Int)(implicit arb: Arbitrary[A]): Gen[List[QM.QueueMethod[A]]] = {
    if (nActions == 0) {
      Gen.const(List.empty[QM.QueueMethod[A]])
    } else {
      for {
        (qm, sNext) <- genQActionAndNextSize(s0)
        qms <- genQActions(sNext, nActions - 1)
      } yield qm +: qms
    }
  }

  def runQAction[A, QA <: api.Queue[A]](as: QA, qact: QM.QueueMethod[A]): Option[QA] = {
    qact match {
      case QueueMethod.QueueEnqueue(x) => Try {
        as.enqueue(x); as
      }.toOption
      case QueueMethod.QueueDequeue() => Try {
        as.dequeue(); as
      }.toOption
    }
  }

  def runQActions[A, QA <: api.Queue[A]](as0: QA, qacts: List[QM.QueueMethod[A]]): Option[QA] = {
    qacts.foldLeft(Option(as0)) { (asOpt, qact) =>
      asOpt.flatMap(runQAction(_, qact))
    }
  }
}
