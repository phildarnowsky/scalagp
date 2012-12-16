import org.specs2.mutable._
import org.specs2.specification.Scope

import com.darnowsky.scalagp.FullGenerationStrategy._
import com.darnowsky.scalagp.ProgramNode._

class FullGenerationStrategySpec extends Specification with SpecHelpers with ProgramGenerationStrategySharedExamples {
  val branchStrategy = new FullGenerationStrategy[Int](List(Nonterminal1, Nonterminal2), List(Terminal1, Terminal2), 3)
  val leafStrategy = new FullGenerationStrategy[Int](List(Nonterminal1, Nonterminal2), List(Terminal1, Terminal2), 2)

  val leafGeneratorTestStrategy = new FullGenerationStrategy[Int](List(NonterminalGenerator1), List(TerminalGenerator1), 2)
  val branchGeneratorTestStrategy = new FullGenerationStrategy[Int](List(NonterminalGenerator1), List(TerminalGenerator1), 3)

  "A FullGenerationStrategy" should {
    generateTerminals(leafGeneratorTestStrategy, Terminal1)
    generateNonterminals(branchGeneratorTestStrategy, Nonterminal1)

    "generate all nonterminal node functions in a branch node" in {
      val result = branchStrategy.generateChildFunctions(10)
      allEvaluationFunctionsIn(result, List(Nonterminal1, Nonterminal2)) must beTrue
      result must have length(10)
    }

    "generate all terminal node functions in a leaf node" in {
      val result = leafStrategy.generateChildFunctions(17)
      allEvaluationFunctionsIn(result, List(Terminal1, Terminal2)) must beTrue
      result must have length(17)
    }
  }
}
