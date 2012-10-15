import org.specs2.mutable._
import org.specs2.specification.Scope

import com.darnowsky.scalagp.ProgramGenerationStrategy._
import com.darnowsky.scalagp.PsuedorandomGenerationStrategy._
import com.darnowsky.scalagp.NodeFunction._
import com.darnowsky.scalagp.ProgramNode.ProgramNode
import com.darnowsky.scalagp.NodeFunction.NodeFunctionCreatorImplicits._

class ProgramGenerationStrategySpec extends Specification {
  class GenericGenerationStrategy[T](
    nonterminals: Seq[NonterminalNodeFunction[T]], 
    terminals: Seq[TerminalNodeFunction[T]]) 
  extends ProgramGenerationStrategy[T](nonterminals, terminals) {
    def generateChildren(depth: Int, arity: Int) = {
      if(isTerminalDepth(depth))
        List(new ProgramNode[T](terminals.head, this, depth - 1))
      else
        List(new ProgramNode[T](nonterminals.head, this, depth - 1))
    }
  }

  object TestTerminal extends TerminalNodeFunction[Int] {
    def apply(children: Seq[ProgramNode[Int]]) = 42
  }

  object TestNonterminal extends NonterminalNodeFunction[Int](1) {
    def apply(children: Seq[ProgramNode[Int]]) = children.head.evaluate
  }

  object TestNonterminalGenerator extends NonterminalNodeFunctionCreator[Int] {
    def getNodeFunction(): NonterminalNodeFunction[Int] = TestNonterminal
  }

  object TestTerminalGenerator extends TerminalNodeFunctionCreator[Int] {
    def getNodeFunction(): TerminalNodeFunction[Int] = TestTerminal
  }

  val testStrategy = new GenericGenerationStrategy[Int](List(TestNonterminalGenerator), List(TestTerminalGenerator))

  "A generic ProgramGenerationStrategy" should {
    "be able to generate nonterminals from an arbitrary object that implements NonterminalNodeFunctionCreator" in { 
      testStrategy.generateChildren(2,1).head.evaluationFunction mustEqual TestNonterminal
    }

    "be able to generate terminals from an arbitrary object that implements TerminalNodeFunctionCreator" in { 
      testStrategy.generateChildren(1,1).head.evaluationFunction mustEqual TestTerminal
    }
  }
}
