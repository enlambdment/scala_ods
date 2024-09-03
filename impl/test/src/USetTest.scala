package impl
import utest._

import cats.kernel.Eq
import impl.USetTestUtils.{genUActions, runUActions}
import org.scalacheck.Prop.forAll
import scala.collection.mutable

object USetTest extends TestSuite {
  def tests = Tests {
    test("ArrayUSet test") {
      val propUSetOps = forAll(genUActions[Int](20)) { ums =>
        val ns: ArrayUSet[Int] = new ArrayUSet[Int]
        val nset: mutable.HashSet[Int] = ns.toScala
        val outcome = runUActions((ns, nset), ums)
        val (finalUSet, finalHashSet) = outcome.finals
        val allOutputs = outcome.outputs
        (finalUSet.toScala == finalHashSet) && (allOutputs.forall({ case (uSetOut, hashSetOut) =>
          Eq.eqv(uSetOut, hashSetOut)
        }))
      }
      propUSetOps.check()
    }
  }
}
