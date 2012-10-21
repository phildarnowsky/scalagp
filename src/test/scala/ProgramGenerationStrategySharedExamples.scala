import org.specs2.mutable._
import org.specs2.specification.Scope

import com.darnowsky.scalagp.ProgramGenerationStrategy._
import com.darnowsky.scalagp.NodeFunction.{TerminalNodeFunction, NonterminalNodeFunction, TerminalNodeFunctionCreator, NonterminalNodeFunctionCreator}
import com.darnowsky.scalagp.ProgramNode.ProgramNode


trait ProgramGenerationStrategySharedExamples extends Specification {
  def generateTerminals[T](
    testStrategy: ProgramGenerationStrategy[T], 
    expectedTerminal: TerminalNodeFunction[T]
  ) = {

    "like any subclass of ProgramGenerationStrategy" should {
      "be able to generate terminals from an arbitrary object that implements TerminalNodeFunctionCreator" in { 
        testStrategy.generateChildren(1,1).head.evaluationFunction mustEqual expectedTerminal
      }
    }
  }

  def generateNonterminals[T](
    testStrategy: ProgramGenerationStrategy[T], 
    expectedNonterminal: NonterminalNodeFunction[T]
  ) = {

    "like any subclass of ProgramGenerationStrategy" should {
      "be able to generate nonterminals from an arbitrary object that implements NonterminalNodeFunctionCreator" in { 
        testStrategy.generateChildren(2,1).head.evaluationFunction mustEqual expectedNonterminal
      }
    }
  }
}
