package impl

import cats.kernel.Eq
import impl.{USetMethod => UM, USetReturn => UR}
import org.scalacheck.{Arbitrary, Gen}

import scala.collection.mutable
import scala.util.Try

/**
 * We require a different strategy for property testing USet's
 * over an element type A.
 *
 * Basically: If we were to allow the elements to be added to
 * some `xs: USet[A]` to be generated in a totally arbitrary
 * manner:
 *
 *  `def badGenUAdd[A](implicit arb: Arbitrary[A]): Gen[UM.USetMethod[A]] = { ... }`
 *
 * then in general it is very unlikely that the `remove` calls
 * generated have an argument which coincides with any of the
 * contents of `xs`. In that case we would rarely hit the case
 * where `remove(y)` is supposed to return `Some(q)` where
 * y, q are equal, and we would not comprehensively test the
 * `remove` behavior for the implementation in question.
 *
 * Therefore, we should generate "up front" a collection of elements,
 *  `(xs, absentXs): (HashSet[A], HashSet[A])`,
 * for the generated `add` / `remove` / `find` calls to vary their argument
 * over. Crucially, we will "hold out" some subset of `testItems` so that they
 * are never added to the `USet[A]` instance being tested, during its "lifetime".
 * That way, we can reasonably count on hitting the case where an unrepresented
 * item is attempted to be removed / found, in the course of running property
 * tests.
 */
object USetTestUtils {
  case class TestItems[A](
    included: Set[A],
    excluded: Set[A]
  )

  def setOfN[T](n: Int, g: Gen[T]): Gen[Set[T]] = {
    val ts: mutable.Set[T] = new mutable.HashSet[T]()
    while (ts.size < n) {
      val tOpt: Option[T] = g.sample
      tOpt.foreach(ts.add)
    }
    Gen.const(ts.toSet)
  }

  def genTestItems[A](n: Int)(implicit arb: Arbitrary[A]): Gen[TestItems[A]] = for {
    as: Set[A] <- setOfN[A](n, Arbitrary.arbitrary[A])
    (included, excluded) = as.splitAt(Math.floor(n * 2 / 3).toInt)
  } yield {
    TestItems(included, excluded)
  }

  def genUAdd[A](items: TestItems[A]): Gen[UM.USetMethod[A]] = for {
    x <- Gen.oneOf(items.included)
  } yield UM.USetAdd(x)

  def genURemove[A](items: TestItems[A]): Gen[UM.USetMethod[A]] = for {
    x <- Gen.oneOf(items.included + items.excluded)
  } yield UM.USetRemove(x)

  def genUFind[A](items: TestItems[A]): Gen[UM.USetMethod[A]] = for {
    x <- Gen.oneOf(items.included + items.excluded)
  } yield UM.USetFind(x)

  def genUAction[A](items: TestItems[A]): Gen[UM.USetMethod[A]] = {
    Gen.frequency(
      (3, genUAdd[A](items)),
      (1, genURemove[A](items)),
      (1, genUFind[A](items)),
    )
  }

  def genUActions[A](items: TestItems[A], nActions: Int): Gen[List[UM.USetMethod[A]]] = {
    val genAction: Gen[UM.USetMethod[A]] = genUAction[A](items)
    Gen.listOfN(nActions, genAction)
  }

  def runUAction[A, UA <: api.USet[A]](as: UA, uact: UM.USetMethod[A]): UR.USetReturn[A] = {
    uact match {
      case USetMethod.USetSize() => UR.USetInt(as.size())
      case USetMethod.USetAdd(x) => UR.USetBoolean(as.add(x))
      case USetMethod.USetRemove(x) => UR.USetAOpt(as.remove(x))
      case USetMethod.USetFind(x) => UR.USetAOpt(as.find(x))
    }
  }

  def runUAction[A : Eq](as: mutable.HashSet[A], uact: UM.USetMethod[A]): UR.USetReturn[A] = {
    uact match {
      case USetMethod.USetSize() => UR.USetInt(as.size)
      case USetMethod.USetAdd(x) => UR.USetBoolean(as.add(x))
      case USetMethod.USetRemove(x) => {
        // simulated api.USet.remove for Scala HashSet
        val matchOpt = as.find(Eq.eqv(_, x))
        matchOpt.foreach(as.remove)
        UR.USetAOpt(matchOpt)
      }
      case USetMethod.USetFind(x) => UR.USetAOpt(as.find(Eq.eqv(_, x)))
    }
  }
}
