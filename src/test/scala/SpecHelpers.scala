import com.darnowsky.scalagp.ProgramNode.ProgramNode
import com.darnowsky.scalagp.NodeFunction._
import com.darnowsky.scalagp.ProgramGenerationStrategy.ProgramGenerationStrategy

trait SpecHelpers {
  def allEvaluationFunctionsIn[T](actual_functions: Seq[NodeFunction[T]], expected_functions: Seq[NodeFunction[T]]): Boolean = {
    actual_functions.forall(expected_functions.contains(_))
  }

  object TerminalGenerator42 extends TerminalNodeFunctionCreator[Int] {
    def getNodeFunction = ConstantEvaluationFunction42
  }

  object NonterminalGeneratorHead extends NonterminalNodeFunctionCreator[Int] {
    def getNodeFunction = HeadEvaluationFunction
  }

  object ConstantEvaluationFunction42 extends TerminalNodeFunction[Int] {
    def apply(xs: Seq[ProgramNode[Int]]) = 42 
    def toIdentifier = "42"
  }

  object ConstantEvaluationFunction666 extends TerminalNodeFunction[Int] {
    def apply(xs: Seq[ProgramNode[Int]]) = 666
    def toIdentifier = "666"
  }

  object ConstantEvaluationFunction1337 extends TerminalNodeFunction[Int] {
    def apply(xs: Seq[ProgramNode[Int]]) = 1337
    def toIdentifier = "1337"
  }

  object ConstantEvaluationFunction3456 extends TerminalNodeFunction[Int] {
    def apply(xs: Seq[ProgramNode[Int]]) = 3456
    def toIdentifier = "3456"
  }

  object ConstantEvaluationFunction78910 extends TerminalNodeFunction[Int] {
    def apply(xs: Seq[ProgramNode[Int]]) = 78910
    def toIdentifier = "78910"
  }

  object HeadEvaluationFunction extends NonterminalNodeFunction[Int](2) {
    def apply(children: Seq[ProgramNode[Int]]) = children.head.evaluate
    def toIdentifier = "head"
  }

  object LastEvaluationFunction extends NonterminalNodeFunction[Int](2) {
    def apply(children: Seq[ProgramNode[Int]]) = children.last.evaluate
    def toIdentifier = "last"
  }


  object AddEvaluationFunction extends NonterminalNodeFunction[Int](2) {
    def apply(nodes: Seq[ProgramNode[Int]]) = nodes.map(_.evaluate).sum
    def toIdentifier = "+"
  }

  object MultiplyEvaluationFunction extends NonterminalNodeFunction[Int](2) {
    def apply(nodes: Seq[ProgramNode[Int]]) = nodes.map(_.evaluate).product
    def toIdentifier = "*"
  }

  object SuccessorEvaluationFunction extends NonterminalNodeFunction[Int](1) {
    def apply(nodes: Seq[ProgramNode[Int]]) = nodes.head.evaluate + 1
    def toIdentifier = "succ"
  }

  object NegateEvaluationFunction extends NonterminalNodeFunction[Int](1) {
    def apply(nodes: Seq[ProgramNode[Int]]) = -(nodes.head.evaluate)
    def toIdentifier = "-"
  }
}
