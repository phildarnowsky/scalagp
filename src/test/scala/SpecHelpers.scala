import com.darnowsky.scalagp.ProgramNode.ProgramNode
import com.darnowsky.scalagp.NodeFunction.{NodeFunction, TerminalNodeFunction, NonterminalNodeFunction, TerminalNodeFunctionCreator, NonterminalNodeFunctionCreator}

trait SpecHelpers {
  def allEvaluationFunctionsIn[T](nodes: Seq[ProgramNode[T]], functions: Seq[NodeFunction[T]]): Boolean = {
    nodes.forall((node: ProgramNode[T]) => functions.contains(node.evaluationFunction))
  }

  object Nonterminal1 extends NonterminalNodeFunction[Int](2) {
    def apply(children: Seq[ProgramNode[Int]]) = children.head.evaluate()
    def toIdentifier = "nonterminal1"
  }

  object Nonterminal2 extends NonterminalNodeFunction[Int](2) {
    def apply(children: Seq[ProgramNode[Int]]) = children.last.evaluate()
    def toIdentifier = "nonterminal2"
  }

  object Terminal1 extends TerminalNodeFunction[Int] {
    def apply(children: Seq[ProgramNode[Int]]) = 42
    def toIdentifier = "terminal1"
  }

  object Terminal2 extends TerminalNodeFunction[Int] {
    def apply(children: Seq[ProgramNode[Int]]) = 666
    def toIdentifier = "terminal2"
  }

  object TerminalGenerator1 extends TerminalNodeFunctionCreator[Int] {
    def getNodeFunction = Terminal1
  }

  object NonterminalGenerator1 extends NonterminalNodeFunctionCreator[Int] {
    def getNodeFunction = Nonterminal1
  }
}
