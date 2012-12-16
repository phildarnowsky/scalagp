import org.specs2.mutable._
import org.specs2.mock._

import com.darnowsky.scalagp.ProgramGenerationStrategy._
import com.darnowsky.scalagp.NodeFunction.{TerminalNodeFunction, NonterminalNodeFunction, NodeFunctionCreator}
import com.darnowsky.scalagp.FullGenerationStrategy.FullGenerationStrategy
import com.darnowsky.scalagp.GrowGenerationStrategy.GrowGenerationStrategy
import com.darnowsky.scalagp.ProgramNode.ProgramNode
import com.darnowsky.scalagp.Population._
import scala.collection.immutable.Queue

class PopulationSpec extends Specification with Mockito {
  object TestProgramFitnessFunction extends ProgramFitnessFunction[Int, Double] {
    def apply(programOutput: Int): Double = programOutput * 3.0
  }

  object Nonterminal1 extends NonterminalNodeFunction[Int](2) {
    def apply(nodes: Seq[ProgramNode[Int]]) = nodes.head.evaluate
    def toIdentifier = "first"
  }

  object Nonterminal2 extends NonterminalNodeFunction[Int](2) {
    def apply(nodes: Seq[ProgramNode[Int]]) = nodes.last.evaluate
    def toIdentifier = "last"
  }

  object Terminal1 extends TerminalNodeFunction[Int] {
    def apply(nodes: Seq[ProgramNode[Int]]) = 123
    def toIdentifier = "123"
  }

  object Terminal2 extends TerminalNodeFunction[Int] {
    def apply(nodes: Seq[ProgramNode[Int]]) = 456
    def toIdentifier = "456"
  }

  "The Population companion object" should {
    "be able to generate a Population via even splits" in {
      val fullStrategy = spy(new FullGenerationStrategy[Int](List(Nonterminal1, Nonterminal2), List(Terminal1, Terminal2), 5))
      val growStrategy = spy(new GrowGenerationStrategy[Int](List(Nonterminal1, Nonterminal2), List(Terminal1, Terminal2), 5))

      val population = Population.generate(3, List(fullStrategy, growStrategy), TestProgramFitnessFunction)

      there was 3.times(fullStrategy).generateProgram
      there was 3.times(growStrategy).generateProgram
    }
  }

  "A Population" should {
    "evaluate its constituent programs for fitness" in {
      val commonChildren = Array(
        new ProgramNode(Terminal1, Array():Array[ProgramNode[Int]], Queue(0)),
        new ProgramNode(Terminal2, Array():Array[ProgramNode[Int]], Queue(1))
      )

      val testProgram1 = new ProgramNode(Nonterminal1, commonChildren, Queue())
      val testProgram2 = new ProgramNode(Nonterminal2, commonChildren, Queue())
      val population = new Population(List(testProgram1, testProgram2), TestProgramFitnessFunction)

      val fitnessResult = population.fitnesses
      fitnessResult.size mustEqual 2
      fitnessResult(testProgram1) mustEqual 369.0
      fitnessResult(testProgram2) mustEqual 1368.0
    }
  }
}
