import org.specs2.mutable._

import com.darnowsky.scalagp.ProgramNode._
import com.darnowsky.scalagp.ProgramGenerationStrategy._
import com.darnowsky.scalagp.NodeFunction.{TerminalNodeFunction, NonterminalNodeFunction}
import org.specs2.mock._

object ConstantEvaluationFunction extends TerminalNodeFunction[Int] {
  def apply(xs: Seq[ProgramNode[Int]]) = 42 
}

object AddEvaluationFunction extends NonterminalNodeFunction[Int](2) {
  def apply(nodes: Seq[ProgramNode[Int]]) = nodes.map(_.evaluate).sum
}

class FakeProgramNode(childrenCreationStrategy: ProgramGenerationStrategy[Int], depth: Int) extends ProgramNode[Int](ConstantEvaluationFunction, childrenCreationStrategy, depth) {
}

class TopProgramNode(childrenCreationStrategy: ProgramGenerationStrategy[Int], depth: Int = 5) extends ProgramNode[Int](ConstantEvaluationFunction, childrenCreationStrategy, depth)

object FakeProgramNode1 extends ProgramNode[Int](ConstantEvaluationFunction, new FakeGenerationStrategy, 1) {
}

object FakeProgramNode2 extends ProgramNode[Int](ConstantEvaluationFunction, new FakeGenerationStrategy, 2) {
}

object AddProgramNode extends ProgramNode(AddEvaluationFunction, new FakeGenerationStrategy, 3) {
}

class FakeGenerationStrategy extends ProgramGenerationStrategy[Int](List(), List()) {
  def generateChildren(depth: Int, arity: Int) = {
    List(FakeProgramNode1, FakeProgramNode2)
  }
}

class ProgramNodeSpec extends Specification with Mockito {
  "A ProgramNode" should {
    "use the children assigned to it by the ProgramGenerationStrategy it's initialized with" in {
      val topNode = new TopProgramNode(new FakeGenerationStrategy)

      topNode.children.length mustEqual 2
      topNode.children(0) mustEqual FakeProgramNode1
      topNode.children(1) mustEqual FakeProgramNode2
    }

    "be able to evaluate itself" in {
      FakeProgramNode1.evaluate() mustEqual 42
    }

    "be able to evaluate itself as a function of its children" in {
      AddProgramNode.evaluate() mustEqual (42 + 42)
    }

    "call generateChildren on its ProgramGenerationStrategy with the correct depth and arity" in {
      val spiedStrategy = spy(new FakeGenerationStrategy)
      val node = new ProgramNode(AddEvaluationFunction, spiedStrategy, 3)
      val kids = node.children
      there was one(spiedStrategy).generateChildren(3, 2)
    }
  }
}
