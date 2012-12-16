import org.specs2.mutable._

import com.darnowsky.scalagp.ProgramNode._
import com.darnowsky.scalagp.ProgramGenerationStrategy._
import com.darnowsky.scalagp.NodeFunction.{TerminalNodeFunction, NonterminalNodeFunction, NodeFunctionCreator}
import org.specs2.mock._
import scala.collection.immutable.Queue

class ProgramNodeSpec extends Specification with Mockito with SpecHelpers {
  "A ProgramNode" should {
    "be able to do crossover reproduction with another ProgramNode" in { pending }  

    "be able to evaluate itself" in {
      val constantNode = new ProgramNode[Int](ConstantEvaluationFunction1, Array():Array[ProgramNode[Int]])
      constantNode.evaluate() mustEqual 42
    }

    "be able to evaluate itself as a function of its children" in {
      val leftChild = new ProgramNode[Int](ConstantEvaluationFunction1, Array():Array[ProgramNode[Int]], Queue(0))
      val rightChild = new ProgramNode[Int](ConstantEvaluationFunction2, Array():Array[ProgramNode[Int]], Queue(1))
      val topNode = new ProgramNode[Int](AddEvaluationFunction, Array(leftChild, rightChild))
      topNode.evaluate() mustEqual (42 + 666)
    }
  }

  "A ProgramNode initialized by way of a ProgramGenerationStrategy" should {
    class FakeProgramGenerationStrategy(depth: Int) extends ProgramGenerationStrategy[Int](List(), List(), depth) {
      def chooseTerminalFunction(): NodeFunctionCreator[Int] = { ConstantEvaluationFunction1 }
      def chooseNonterminalFunction(): NodeFunctionCreator[Int] = { AddEvaluationFunction }
      def successor(): FakeProgramGenerationStrategy = new FakeProgramGenerationStrategy(depth - 1)
    }

    val topNode = new ProgramNode[Int](
      AddEvaluationFunction,
      new FakeProgramGenerationStrategy(3),
      Queue()
    )


    "use the children assigned to it by the ProgramGenerationStrategy it's initialized with" in {
      topNode.children.length mustEqual 2
      topNode.children.forall(_.evaluationFunction == AddEvaluationFunction) must beTrue

      val grandchildren = topNode.children.flatMap(_.children)
      grandchildren.length mustEqual 4
      grandchildren.forall(_.evaluationFunction == ConstantEvaluationFunction1) must beTrue
    }

    "know the path from the root to here" in {
      topNode.pathFromRoot mustEqual Queue()
      topNode.children(0).pathFromRoot mustEqual Queue(0)
      topNode.children(1).pathFromRoot mustEqual Queue(1)

      topNode.children(0).children(0).pathFromRoot mustEqual Queue(0, 0)
      topNode.children(0).children(1).pathFromRoot mustEqual Queue(0, 1)
      topNode.children(1).children(0).pathFromRoot mustEqual Queue(1, 0)
      topNode.children(1).children(1).pathFromRoot mustEqual Queue(1, 1)
    }
  }
}
