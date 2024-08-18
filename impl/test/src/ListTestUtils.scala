package impl

import impl.{ListMethod => LM}
import org.scalacheck.{Arbitrary, Gen}

import scala.collection.mutable
import scala.util.Try

object ListTestUtils {
  def genLGet[A](s: Int): Gen[LM.ListMethod[A]] = for {
    i <- Gen.choose(0, s - 1)
  } yield LM.ListGet(i)

  def genLSet[A](s: Int)(implicit arb: Arbitrary[A]): Gen[LM.ListMethod[A]] = for {
    i <- Gen.choose(0, s - 1)
    x <- Arbitrary.arbitrary[A]
  } yield LM.ListSet(i, x)

  def genLAdd[A](s: Int)(implicit arb: Arbitrary[A]): Gen[LM.ListMethod[A]] = for {
    i <- Gen.choose(0, s)
    x <- Arbitrary.arbitrary[A]
  } yield LM.ListAdd(i, x)

  def genLRemove[A](s: Int): Gen[LM.ListMethod[A]] = for {
    i <- Gen.choose(0, s - 1)
  } yield LM.ListRemove(i)

  def genLActionAndNextSize[A](s: Int)(implicit arb: Arbitrary[A]): Gen[(LM.ListMethod[A], Int)] =
    if (s > 0) {
      Gen.frequency(
        (1, genLGet[A](s).map(lm => (lm, s))),
        (1, genLSet[A](s).map(lm => (lm, s))),
        (2, genLAdd[A](s).map(lm => (lm, s + 1))),
        (1, genLRemove[A](s).map(lm => (lm, s - 1)))
      )
    } else {
      genLAdd[A](s).map(lm => (lm, s + 1))
    }

  def genLActions[A](s0: Int, nActions: Int)(implicit arb: Arbitrary[A]): Gen[List[LM.ListMethod[A]]] = {
    if (nActions == 0) {
      Gen.const(List.empty[LM.ListMethod[A]])
    } else {
      for {
        (lm, sNext) <- genLActionAndNextSize(s0)
        lms <- genLActions(sNext, nActions - 1)
      } yield lm +: lms
    }
  }

  def runLAction[A, LA <: api.List[A]](as: LA, lact: LM.ListMethod[A]): Option[LA] = {
    lact match {
      case LM.ListSize() => Some(as)
      case LM.ListGet(i) => Try {
        as.get(i); as
      }.toOption
      case LM.ListSet(i, x) => Try {
        as.set(i, x); as
      }.toOption
      case LM.ListAdd(i, x) => Try {
        as.add(i, x); as
      }.toOption
      case LM.ListRemove(i) => Try {
        as.remove(i); as
      }.toOption
    }
  }

  def runLAction[A](as: mutable.ListBuffer[A], lact: LM.ListMethod[A]): Option[mutable.ListBuffer[A]] = {
    lact match {
      case ListMethod.ListSize() => Some(as)
      case ListMethod.ListGet(i) => Try {
        as(i); as
      }.toOption
      case ListMethod.ListSet(i, x) => Try {
        as(i) = x; as
      }.toOption
      case ListMethod.ListAdd(i, x) => Try {
        as.insert(i, x); as
      }.toOption
      case ListMethod.ListRemove(i) => Try {
        as.remove(i); as
      }.toOption
    }
  }

  def runLActions[A, LA <: api.List[A]](
    listObjects0: (LA, mutable.ListBuffer[A]),
    lacts: List[LM.ListMethod[A]]
  ): Option[(LA, mutable.ListBuffer[A])] = {
    lacts.foldLeft(Option(listObjects0)) { case (listObjectsOpt, lact) =>
      for {
        (xs, xbuff) <- listObjectsOpt
        ys    <- runLAction(xs, lact)
        ybuff <- runLAction(xbuff, lact)
      } yield {
        (ys, ybuff)
      }
    }
  }
}
