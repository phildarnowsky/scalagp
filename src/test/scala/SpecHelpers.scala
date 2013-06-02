import com.darnowsky.scalagp.ProgramNode.ProgramNode
import com.darnowsky.scalagp.NodeFunction._
import com.darnowsky.scalagp.ProgramGenerationStrategy.ProgramGenerationStrategy

import scala.collection.immutable.HashMap

trait SpecHelpers {
  def allEvaluationFunctionsIn[T](actual_functions: Seq[NodeFunction[T]], expected_functions: Seq[NodeFunction[T]]): Boolean = {
    actual_functions.forall(expected_functions.contains(_))
  }

  def intsToProgramFitnessMap(numericFitnesses: List[Int]): HashMap[ProgramNode[Int], Double] = {
  numericFitnesses.foldLeft(HashMap[ProgramNode[Int], Double]()) {(fitnesses: HashMap[ProgramNode[Int], Double], numericFitness: Int) =>
    val program = new ProgramNode[Int](new ConstantEvaluationFunction(numericFitness))
    fitnesses + (program -> numericFitness.toDouble)
  }
}

object TerminalGenerator42 extends TerminalNodeFunctionCreator[Int] {
    def getNodeFunction = ConstantEvaluationFunction42
  }

  object NonterminalGeneratorHead extends NonterminalNodeFunctionCreator[Int] {
    def getNodeFunction = HeadEvaluationFunction
  }

  class ConstantEvaluationFunction(constant: Int) extends TerminalNodeFunction[Int] {
    def apply(xs: Seq[ProgramNode[Int]]) = constant
    def toIdentifier = constant.toString
  }

  object ConstantEvaluationFunction42 extends ConstantEvaluationFunction(42) {
  }

  object ConstantEvaluationFunction666 extends ConstantEvaluationFunction(666) {
  }

  object ConstantEvaluationFunction1337 extends ConstantEvaluationFunction(1337) {
  }

  object ConstantEvaluationFunction3456 extends ConstantEvaluationFunction(3456) {
  }

  object ConstantEvaluationFunction78910 extends ConstantEvaluationFunction(78910) {
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
