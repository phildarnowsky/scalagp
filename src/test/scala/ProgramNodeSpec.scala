import org.specs2.mutable._

import com.darnowsky.scalagp.ProgramNode._
import com.darnowsky.scalagp.ProgramGenerationStrategy._
import com.darnowsky.scalagp.NodeFunction.{TerminalNodeFunction, NonterminalNodeFunction}
import org.specs2.mock._

object ConstantEvaluationFunction1 extends TerminalNodeFunction[Int] {
  def apply(xs: Seq[ProgramNode[Int]]) = 42 
  def toIdentifier = "42"
}

object ConstantEvaluationFunction2 extends TerminalNodeFunction[Int] {
  def apply(xs: Seq[ProgramNode[Int]]) = 666
  def toIdentifier = "666"
}

object AddEvaluationFunction extends NonterminalNodeFunction[Int](2) {
  def apply(nodes: Seq[ProgramNode[Int]]) = nodes.map(_.evaluate).sum
  def toIdentifier = "+"
}

class FakeProgramNode(childrenCreationStrategy: ProgramGenerationStrategy[Int], depth: Int, parent: Option[ProgramNode[Int]]) extends ProgramNode[Int](ConstantEvaluationFunction1, childrenCreationStrategy, depth, parent) {
}

class TopProgramNode(childrenCreationStrategy: ProgramGenerationStrategy[Int], depth: Int = 5) extends ProgramNode[Int](AddEvaluationFunction, childrenCreationStrategy, depth, None)

class FakeGenerationStrategy extends ProgramGenerationStrategy[Int](List(), List()) {
  var callCount = 0

  override def generateChildFunctions(terminalDepth: Boolean, arity: Int) = {
    List(ConstantEvaluationFunction1, ConstantEvaluationFunction2)
  }

  def chooseTerminalFunction() = ConstantEvaluationFunction1
  def chooseNonterminalFunction() = AddEvaluationFunction
}

class ProgramNodeSpec extends Specification with Mockito {
  "A ProgramNode" should {
    "use the children assigned to it by the ProgramGenerationStrategy it's initialized with" in {
      val topNode = new TopProgramNode(new FakeGenerationStrategy)

      topNode.children.length mustEqual 2
      topNode.children(0).evaluationFunction mustEqual ConstantEvaluationFunction1
      topNode.children(1).evaluationFunction mustEqual ConstantEvaluationFunction2
    }

    "know who its parent is" in { pending }

    "know its index within its parent" in { pending }

    "be able to evaluate itself" in {
      val constantNode = new ProgramNode[Int](ConstantEvaluationFunction1, new FakeGenerationStrategy, 1, None)
      constantNode.evaluate() mustEqual 42
    }

    "be able to evaluate itself as a function of its children" in {
      val topNode = new TopProgramNode(new FakeGenerationStrategy)

      topNode.evaluate() mustEqual (42 + 666)
    }

    "call generateChildFunctions on its ProgramGenerationStrategy with the correct depth flag and arity" in {
      val spiedStrategy = spy(new FakeGenerationStrategy)
      val node = new ProgramNode(AddEvaluationFunction, spiedStrategy, 3, None)
      val kids = node.children
      there was one(spiedStrategy).generateChildFunctions(false, AddEvaluationFunction.arity)
    }
  }
}
