import org.specs2.mutable._
import org.specs2.specification.Scope

import com.darnowsky.scalagp.FullGenerationStrategy._
import com.darnowsky.scalagp.ProgramNode._

class FullGenerationStrategySpec extends Specification with SpecHelpers with ProgramGenerationStrategySharedExamples {
  val testStrategy = new FullGenerationStrategy[Int](List(Nonterminal1, Nonterminal2), List(Terminal1, Terminal2))

  val generatorTestStrategy = new FullGenerationStrategy[Int](List(NonterminalGenerator1), List(TerminalGenerator1))

  "A FullGenerationStrategy" should {
    generateTerminals(generatorTestStrategy, Terminal1)
    generateNonterminals(generatorTestStrategy, Nonterminal1)

    "generate all nonterminal node functions in a branch node" in {
      val result = testStrategy.generateChildFunctions(false, 10)
      allEvaluationFunctionsIn(result, List(Nonterminal1, Nonterminal2)) must beTrue
      result must have length(10)
    }

    "generate all terminal node functions in a leaf node" in {
      val result = testStrategy.generateChildFunctions(true, 17)
      allEvaluationFunctionsIn(result, List(Terminal1, Terminal2)) must beTrue
      result must have length(17)
    }
  }
}
