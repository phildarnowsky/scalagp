import com.darnowsky.scalagp.ProgramNode.ProgramNode
import com.darnowsky.scalagp.NodeFunction.{NodeFunction, TerminalNodeFunction, NonterminalNodeFunction}

trait SpecHelpers {
  def allEvaluationFunctionsIn[T](nodes: Seq[ProgramNode[T]], functions: Seq[NodeFunction[T]]): Boolean = {
    nodes.forall((node: ProgramNode[T]) => functions.contains(node.evaluationFunction))
  }

  object Nonterminal1 extends NonterminalNodeFunction[Int](2) {
    def apply(children: Seq[ProgramNode[Int]]) = children.head.evaluate()
  }

  object Nonterminal2 extends NonterminalNodeFunction[Int](2) {
    def apply(children: Seq[ProgramNode[Int]]) = children.last.evaluate()
  }

  object Terminal1 extends TerminalNodeFunction[Int] {
    def apply(children: Seq[ProgramNode[Int]]) = 42
  }

  object Terminal2 extends TerminalNodeFunction[Int] {
    def apply(children: Seq[ProgramNode[Int]]) = 666
  }

}
