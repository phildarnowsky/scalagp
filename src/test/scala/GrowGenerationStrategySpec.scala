import org.specs2.mutable._
import org.specs2.specification.Scope

import com.darnowsky.scalagp.GrowGenerationStrategy._
import com.darnowsky.scalagp.ProgramNode._

class GrowGenerationStrategySpec extends Specification with SpecHelpers {
  val testStrategy = new GrowGenerationStrategy[Int](List(Nonterminal1, Nonterminal2), List(Terminal1, Terminal2))
  val terminalOnlyStrategy = new GrowGenerationStrategy[Int](List(), List(Terminal1, Terminal2))

  "A GrowGenerationStrategy" should {
    "generate either terminal or nonterminal nodes in a branch node" in {
      val result1 = testStrategy.generateChildren(2, 3)
      val result2 = terminalOnlyStrategy.generateChildren(2, 3)

      allEvaluationFunctionsIn(result1, List(Nonterminal1, Nonterminal2, Terminal1, Terminal2)) must beTrue
      allEvaluationFunctionsIn(result2, List(Terminal1, Terminal2)) must beTrue

      result1 must have length(3)
      result2 must have length(3)
    }

    // TODO: figure out how to factor this out between this and the full
    // strategy spec, maybe write an abstract program generation strategy spec
    "initialize all children of a branch node it generates with itself and with decremented depth" in {
      val result = testStrategy.generateChildren(5,17)
      result.forall((node: ProgramNode[Int]) => node.depth == 4) must beTrue
      result.forall((node: ProgramNode[Int]) => node.childrenCreationStrategy == testStrategy) must beTrue
    }

    "generate all terminal nodes in a leaf node" in {
      val result1 = testStrategy.generateChildren(1, 3)
      val result2 = terminalOnlyStrategy.generateChildren(1, 3)

      allEvaluationFunctionsIn(result1, List(Terminal1, Terminal2)) must beTrue
      allEvaluationFunctionsIn(result2, List(Terminal1, Terminal2)) must beTrue

      result1 must have length(3)
      result2 must have length(3)
    }
  }
}
