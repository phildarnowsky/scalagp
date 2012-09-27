import org.specs2.mutable._

import com.darnowsky.scalagp.ProgramNode._
import com.darnowsky.scalagp.ProgramGenerationStrategy._

object ConstantEvaluationFunction extends Function1[Seq[ProgramNode[Int]], Int] {
  def apply(xs: Seq[ProgramNode[Int]]) = 42 
}

object AddEvaluationFunction extends Function1[Seq[ProgramNode[Int]], Int] {
  def apply(nodes: Seq[ProgramNode[Int]]) = nodes.map(_.evaluate).sum
}

class FakeProgramNode(childrenCreationStrategy: ProgramGenerationStrategy[Int], depth: Int) extends ProgramNode[Int](ConstantEvaluationFunction, childrenCreationStrategy, depth) {
}

class TopProgramNode(childrenCreationStrategy: ProgramGenerationStrategy[Int], depth: Int = 0) extends ProgramNode[Int](ConstantEvaluationFunction, childrenCreationStrategy, depth)

object FakeProgramNode1 extends ProgramNode[Int](ConstantEvaluationFunction, new FakeGenerationStrategy, 1) {
}

object FakeProgramNode2 extends ProgramNode[Int](ConstantEvaluationFunction, new FakeGenerationStrategy, 2) {
}

object AddProgramNode extends ProgramNode(AddEvaluationFunction, new FakeGenerationStrategy, 1) {
}

class FakeGenerationStrategy extends ProgramGenerationStrategy[Int] {
  def generateChildren(depth: Int) = {
    List(FakeProgramNode1, FakeProgramNode2)
  }
}

class ProgramNodeSpec extends Specification {
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
  }
}
