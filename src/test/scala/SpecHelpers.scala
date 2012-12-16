import com.darnowsky.scalagp.ProgramNode.ProgramNode
import com.darnowsky.scalagp.NodeFunction._
import com.darnowsky.scalagp.ProgramGenerationStrategy.ProgramGenerationStrategy

trait SpecHelpers {
  def allEvaluationFunctionsIn[T](actual_functions: Seq[NodeFunction[T]], expected_functions: Seq[NodeFunction[T]]): Boolean = {
    actual_functions.forall(expected_functions.contains(_))
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

  object ConstantEvaluationFunction1 extends TerminalNodeFunction[Int] {
    def apply(xs: Seq[ProgramNode[Int]]) = 42 
    def toIdentifier = "42"
  }

  object ConstantEvaluationFunction2 extends TerminalNodeFunction[Int] {
    def apply(xs: Seq[ProgramNode[Int]]) = 666
    def toIdentifier = "666"
  }

  object AddEvaluationFunction extends NonterminalNodeFunction[Int](2) {
    def apply(nodes: Seq[ProgramNode[Int]]) = nodes.map(_.evaluate).sum
    def toIdentifier = "+"
  }
}
