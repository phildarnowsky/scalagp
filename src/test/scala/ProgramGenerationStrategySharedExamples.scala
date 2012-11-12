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
      "be able to generate terminal functions from an arbitrary object that implements TerminalNodeFunctionCreator" in { 
        testStrategy.generateChildFunctions(true, 1).head mustEqual expectedTerminal
      }
    }
  }

  def generateNonterminals[T](
    testStrategy: ProgramGenerationStrategy[T], 
    expectedNonterminal: NonterminalNodeFunction[T]
  ) = {

    "like any subclass of ProgramGenerationStrategy" should {
      "be able to generate nonterminal functions from an arbitrary object that implements NonterminalNodeFunctionCreator" in { 
        testStrategy.generateChildFunctions(false, 1).head mustEqual expectedNonterminal
      }
    }
  }
}
