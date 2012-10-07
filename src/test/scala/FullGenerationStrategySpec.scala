import org.specs2.mutable._
import org.specs2.specification.Scope

import com.darnowsky.scalagp.FullGenerationStrategy._
import com.darnowsky.scalagp.ProgramNode._

class FullGenerationStrategySpec extends Specification with SpecHelpers {
  val testStrategy = new FullGenerationStrategy[Int](List(Nonterminal1, Nonterminal2), List(Terminal1, Terminal2))

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
