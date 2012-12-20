import org.specs2.mutable._
import org.specs2.specification.Scope

import com.darnowsky.scalagp.GrowGenerationStrategy._
import com.darnowsky.scalagp.ProgramNode._

class GrowGenerationStrategySpec extends Specification with SpecHelpers with ProgramGenerationStrategySharedExamples {
  val branchNodeStrategy = new GrowGenerationStrategy[Int](List(HeadEvaluationFunction, LastEvaluationFunction), List(ConstantEvaluationFunction42, ConstantEvaluationFunction666), 3)
  val leafNodeStrategy = new GrowGenerationStrategy[Int](List(HeadEvaluationFunction, LastEvaluationFunction), List(ConstantEvaluationFunction42, ConstantEvaluationFunction666), 2)
  val terminalOnlyStrategy = new GrowGenerationStrategy[Int](List(), List(ConstantEvaluationFunction42, ConstantEvaluationFunction666), 2)

  val nonterminalGeneratorTestStrategy = new GrowGenerationStrategy[Int](List(NonterminalGeneratorHead), List(), 3)
  val terminalGeneratorTestStrategy = new GrowGenerationStrategy[Int](List(NonterminalGeneratorHead), List(TerminalGenerator42), 2)

  "A GrowGenerationStrategy" should {
    generateTerminals(terminalGeneratorTestStrategy, ConstantEvaluationFunction42)
    generateNonterminals(nonterminalGeneratorTestStrategy, HeadEvaluationFunction)

    "generate either terminal or nonterminal nodes in a branch node" in {
      val result1 = branchNodeStrategy.generateChildFunctions(3)
      val result2 = terminalOnlyStrategy.generateChildFunctions(3)

      allEvaluationFunctionsIn(result1, List(HeadEvaluationFunction, LastEvaluationFunction, ConstantEvaluationFunction42, ConstantEvaluationFunction666)) must beTrue
      allEvaluationFunctionsIn(result2, List(ConstantEvaluationFunction42, ConstantEvaluationFunction666)) must beTrue

      result1 must have length(3)
      result2 must have length(3)
    }

    "generate all terminal node functions in a leaf node" in {
      val result = leafNodeStrategy.generateChildFunctions(3)

      allEvaluationFunctionsIn(result, List(ConstantEvaluationFunction42, ConstantEvaluationFunction666)) must beTrue

      result must have length(3)
    }
  }
}
