import org.specs2.mutable._
import org.specs2.specification.Scope

import com.darnowsky.scalagp.FullGenerationStrategy._
import com.darnowsky.scalagp.ProgramNode._

class FullGenerationStrategySpec extends Specification with SpecHelpers with ProgramGenerationStrategySharedExamples {
  val branchStrategy = new FullGenerationStrategy[Int](List(HeadEvaluationFunction, LastEvaluationFunction), List(ConstantEvaluationFunction42, ConstantEvaluationFunction666), 3)
  val leafStrategy = new FullGenerationStrategy[Int](List(HeadEvaluationFunction, LastEvaluationFunction), List(ConstantEvaluationFunction42, ConstantEvaluationFunction666), 2)

  val leafGeneratorTestStrategy = new FullGenerationStrategy[Int](List(NonterminalGeneratorHead), List(TerminalGenerator42), 2)
  val branchGeneratorTestStrategy = new FullGenerationStrategy[Int](List(NonterminalGeneratorHead), List(TerminalGenerator42), 3)

  "A FullGenerationStrategy" should {
    generateTerminals(leafGeneratorTestStrategy, ConstantEvaluationFunction42)
    generateNonterminals(branchGeneratorTestStrategy, HeadEvaluationFunction)

    "generate all nonterminal node functions in a branch node" in {
      val result = branchStrategy.generateChildFunctions(10)
      allEvaluationFunctionsIn(result, List(HeadEvaluationFunction, LastEvaluationFunction)) must beTrue
      result must have length(10)
    }

    "generate all terminal node functions in a leaf node" in {
      val result = leafStrategy.generateChildFunctions(17)
      allEvaluationFunctionsIn(result, List(ConstantEvaluationFunction42, ConstantEvaluationFunction666)) must beTrue
      result must have length(17)
    }
  }
}
