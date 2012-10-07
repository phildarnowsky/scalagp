import org.specs2.mutable._
import org.specs2.specification.Scope
import org.specs2.mock.Mockito
import org.mockito.Matchers._

import com.darnowsky.scalagp.FullGenerationStrategy._
import com.darnowsky.scalagp.ProgramNode._
import com.darnowsky.scalagp.NodeFunction.{NodeFunction, TerminalNodeFunction, NonterminalNodeFunction}

class FullGenerationStrategySpec extends Specification with Mockito {
  object Nonterminal1 extends NonterminalNodeFunction[Int](2) {
    def apply(children: Seq[ProgramNode[Int]]) = children.head.evaluate()
  }

  object Nonterminal2 extends NonterminalNodeFunction[Int](2) {
    def apply(children: Seq[ProgramNode[Int]]) = children.last.evaluate()
  }

  object Terminal1 extends TerminalNodeFunction[Int] {
    def apply(children: Seq[ProgramNode[Int]]) = 42
  }

  object Terminal2 extends TerminalNodeFunction[Int] {
    def apply(children: Seq[ProgramNode[Int]]) = 666
  }

  val testStrategy = new FullGenerationStrategy[Int](List(Nonterminal1, Nonterminal2), List(Terminal1, Terminal2))

  def allEvaluationFunctionsIn[T](nodes: Seq[ProgramNode[T]], functions: Seq[NodeFunction[T]]): Boolean = {
    nodes.forall((node: ProgramNode[T]) => functions.contains(node.evaluationFunction))
  }

  "A FullGenerationStrategy" should {
    "generate all nonterminal nodes in a branch node" in {
      val result = testStrategy.generateChildren(2, 3)
      allEvaluationFunctionsIn(result, List(Nonterminal1, Nonterminal2)) must beTrue
      result must have length(3)
    }

    "initialize all children of a branch node it generates with itself and with decremented depth" in {
      val result = testStrategy.generateChildren(5,17)
      result.forall((node: ProgramNode[Int]) => node.depth == 4) must beTrue
      result.forall((node: ProgramNode[Int]) => node.childrenCreationStrategy == testStrategy) must beTrue
    }

    "generate all terminal nodes in a leaf node" in {
      val result = testStrategy.generateChildren(1, 3)
      allEvaluationFunctionsIn(result, List(Terminal1, Terminal2)) must beTrue
      result must have length(3)
    }
  }
}
