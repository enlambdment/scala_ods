package impl
import utest._
import impl.ListTestUtils.{genLActions, runLActions}
import impl.QueueTestUtils.{genQActions, runQActions}
import org.scalacheck.Prop.forAll
import scala.collection.mutable

// ./mill -i impl.test.console
object ArrayBasedTest extends TestSuite {
  def tests = Tests {
    test("ArrayStack test") {
      val propListOps = forAll(genLActions[Int](0, 20)) { lms =>
        val ns: ArrayStack[Int] = new ArrayStack[Int]
        val nbuff: mutable.ListBuffer[Int] = ns.toScalaListBuffer
        val resultOpt = runLActions((ns, nbuff), lms)
        resultOpt.isDefined && resultOpt.exists { case (resultList, resultBuff) =>
          resultList.toScalaListBuffer == resultBuff
        }
      }
      propListOps.check()
    }

    test("FastArrayStack test") {
      val propListOps = forAll(genLActions[Int](0, 20)) { lms =>
        val ns: FastArrayStack[Int] = new FastArrayStack[Int]
        val nbuff: mutable.ListBuffer[Int] = ns.toScalaListBuffer
        val resultOpt = runLActions((ns, nbuff), lms)
        resultOpt.isDefined && resultOpt.exists { case (resultList, resultBuff) =>
          resultList.toScalaListBuffer == resultBuff
        }
      }
      propListOps.check()
    }

    test("ArrayQueue test") {
      val propListOps = forAll(genQActions[Int](0, 20)) { qms =>
        val ns: ArrayQueue[Int] = new ArrayQueue[Int]
        val nq: mutable.Queue[Int] = ns.toScalaQueue
        val resultOpt = runQActions((ns, nq), qms)
        resultOpt.isDefined && resultOpt.exists { case (resultQueue, resultScalaQueue) =>
          resultQueue.toScalaQueue == resultScalaQueue
        }
      }
      propListOps.check()
    }

    test("ArrayDeque test") {
      val propListOps = forAll(genLActions[Int](0, 20)) { lms =>
        val ns: ArrayDeque[Int] = new ArrayDeque[Int]
        val nbuff: mutable.ListBuffer[Int] = ns.toScalaListBuffer
        val resultOpt = runLActions((ns, nbuff), lms)
        resultOpt.isDefined && resultOpt.exists { case (resultList, resultBuff) =>
          resultList.toScalaListBuffer == resultBuff
        }
      }
      propListOps.check()
    }

    test("DualArrayDeque test") {
      val propListOps = forAll(genLActions[Int](0, 20)) { lms =>
        val ns: DualArrayDeque[Int] = new DualArrayDeque[Int]
        val nbuff: mutable.ListBuffer[Int] = ns.toScalaListBuffer
        val resultOpt = runLActions((ns, nbuff), lms)
        resultOpt.isDefined && resultOpt.exists { case (resultList, resultBuff) =>
          resultList.toScalaListBuffer == resultBuff
        }
      }
      propListOps.check()
    }

    test("RootishArrayStack test") {
      val propListOps = forAll(genLActions[Int](0, 20)) { lms =>
        val ns: RootishArrayStack[Int] = new RootishArrayStack[Int]
        val nbuff: mutable.ListBuffer[Int] = ns.toScalaListBuffer
        val resultOpt = runLActions((ns, nbuff), lms)
        resultOpt.isDefined && resultOpt.exists { case (resultList, resultBuff) =>
          resultList.toScalaListBuffer == resultBuff
        }
      }
      propListOps.check()
    }
  }
}