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
      val ns: ArrayStack[Int] = new ArrayStack[Int]
      val propListOps = forAll(genLActions[Int](ns.size(), 20)) { lms =>
        val nbuff: mutable.ListBuffer[Int] = ns.toScalaListBuffer
        val resultOpt = runLActions((ns, nbuff), lms)
        resultOpt.isDefined && resultOpt.exists { case (resultList, resultBuff) =>
          resultList.toScalaListBuffer == resultBuff
        }
      }
      propListOps.check()
    }

    test("FastArrayStack test") {
      val ns: FastArrayStack[Int] = new FastArrayStack[Int]
      val propListOps = forAll(genLActions[Int](ns.size(), 20)) { lms =>
        val nbuff: mutable.ListBuffer[Int] = ns.toScalaListBuffer
        val resultOpt = runLActions((ns, nbuff), lms)
        resultOpt.isDefined && resultOpt.exists { case (resultList, resultBuff) =>
          resultList.toScalaListBuffer == resultBuff
        }
      }
      propListOps.check()
    }

    test("ArrayQueue test") {
      val ns: ArrayQueue[Int] = new ArrayQueue[Int]
      val propListOps = forAll(genQActions[Int](ns.size(), 20)) { qms =>
        val nq: mutable.Queue[Int] = ns.toScalaQueue
        val resultOpt = runQActions((ns, nq), qms)
        resultOpt.isDefined && resultOpt.exists { case (resultQueue, resultScalaQueue) =>
          resultQueue.toScalaQueue == resultScalaQueue
        }
      }
      propListOps.check()
    }

    test("ArrayDeque test") {
      val ns: ArrayDeque[Int] = new ArrayDeque[Int]
      val propListOps = forAll(genLActions[Int](ns.size(), 20)) { lms =>
        val nbuff: mutable.ListBuffer[Int] = ns.toScalaListBuffer
        val resultOpt = runLActions((ns, nbuff), lms)
        resultOpt.isDefined && resultOpt.exists { case (resultList, resultBuff) =>
          resultList.toScalaListBuffer == resultBuff
        }
      }
      propListOps.check()
    }

    test("DualArrayDeque test") {
      val ns: DualArrayDeque[Int] = new DualArrayDeque[Int]
      val propListOps = forAll(genLActions[Int](ns.size(), 20)) { lms =>
        val nbuff: mutable.ListBuffer[Int] = ns.toScalaListBuffer
        val resultOpt = runLActions((ns, nbuff), lms)
        resultOpt.isDefined && resultOpt.exists { case (resultList, resultBuff) =>
          resultList.toScalaListBuffer == resultBuff
        }
      }
      propListOps.check()
    }

    test("RootishArrayStack test") {
      val ns: RootishArrayStack[Int] = new RootishArrayStack[Int]
      val propListOps = forAll(genLActions[Int](ns.size(), 20)) { lms =>
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