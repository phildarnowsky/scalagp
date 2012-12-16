import org.specs2.mutable._
import org.specs2.specification.Scope

import com.darnowsky.scalagp.GrowGenerationStrategy._
import com.darnowsky.scalagp.ProgramNode._

class GrowGenerationStrategySpec extends Specification with SpecHelpers with ProgramGenerationStrategySharedExamples {
  val branchNodeStrategy = new GrowGenerationStrategy[Int](List(Nonterminal1, Nonterminal2), List(Terminal1, Terminal2), 3)
  val leafNodeStrategy = new GrowGenerationStrategy[Int](List(Nonterminal1, Nonterminal2), List(Terminal1, Terminal2), 2)
  val terminalOnlyStrategy = new GrowGenerationStrategy[Int](List(), List(Terminal1, Terminal2), 2)

  val nonterminalGeneratorTestStrategy = new GrowGenerationStrategy[Int](List(NonterminalGenerator1), List(), 3)
  val terminalGeneratorTestStrategy = new GrowGenerationStrategy[Int](List(NonterminalGenerator1), List(TerminalGenerator1), 2)

  "A GrowGenerationStrategy" should {
    generateTerminals(terminalGeneratorTestStrategy, Terminal1)
    generateNonterminals(nonterminalGeneratorTestStrategy, Nonterminal1)

    "generate either terminal or nonterminal nodes in a branch node" in {
      val result1 = branchNodeStrategy.generateChildFunctions(3)
      val result2 = terminalOnlyStrategy.generateChildFunctions(3)

      allEvaluationFunctionsIn(result1, List(Nonterminal1, Nonterminal2, Terminal1, Terminal2)) must beTrue
      allEvaluationFunctionsIn(result2, List(Terminal1, Terminal2)) must beTrue

      result1 must have length(3)
      result2 must have length(3)
    }

    "generate all terminal node functions in a leaf node" in {
      val result = leafNodeStrategy.generateChildFunctions(3)

      allEvaluationFunctionsIn(result, List(Terminal1, Terminal2)) must beTrue

      result must have length(3)
    }
  }
}
