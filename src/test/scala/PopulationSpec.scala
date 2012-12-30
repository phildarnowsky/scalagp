import org.specs2.mutable._
import org.specs2.mock._

import com.darnowsky.scalagp.ProgramGenerationStrategy._
import com.darnowsky.scalagp.NodeFunction.{TerminalNodeFunction, NonterminalNodeFunction, NodeFunctionCreator}
import com.darnowsky.scalagp.FullGenerationStrategy.FullGenerationStrategy
import com.darnowsky.scalagp.GrowGenerationStrategy.GrowGenerationStrategy
import com.darnowsky.scalagp.ProgramNode.ProgramNode
import com.darnowsky.scalagp.Population._
import scala.collection.immutable.Queue

class PopulationSpec extends Specification with SpecHelpers with Mockito {
  object TestProgramFitnessFunction extends ProgramFitnessFunction[Int] {
    def apply(programOutput: Int): Double = programOutput * 3.0
  }

  "The Population companion object" should {
    "be able to generate a Population via even splits" in {
      val fullStrategy = spy(new FullGenerationStrategy[Int](List(HeadEvaluationFunction, LastEvaluationFunction), List(ConstantEvaluationFunction42, ConstantEvaluationFunction666), 5))
      val growStrategy = spy(new GrowGenerationStrategy[Int](List(HeadEvaluationFunction, LastEvaluationFunction), List(ConstantEvaluationFunction42, ConstantEvaluationFunction666), 5))

      val population = Population.generate(3, List(fullStrategy, growStrategy), TestProgramFitnessFunction)

      there was 3.times(fullStrategy).generateProgram
      there was 3.times(growStrategy).generateProgram
    }
  }

  "A Population" should {
    "evaluate its constituent programs for fitness" in {
      val commonChildren = Array(
        new ProgramNode(ConstantEvaluationFunction42, Array():Array[ProgramNode[Int]], Queue(0)),
        new ProgramNode(ConstantEvaluationFunction666, Array():Array[ProgramNode[Int]], Queue(1))
      )

      val testProgram1 = new ProgramNode(HeadEvaluationFunction, commonChildren, Queue())
      val testProgram2 = new ProgramNode(LastEvaluationFunction, commonChildren, Queue())
      val population = new Population(List(testProgram1, testProgram2), TestProgramFitnessFunction)

      val fitnessResult = population.fitnesses
      fitnessResult.size mustEqual 2
      fitnessResult(testProgram1) mustEqual 126.0
      fitnessResult(testProgram2) mustEqual 1998.0
    }

    "pick a program for reproduction proportionate to its fitness" in {
      pending
    }
  }
}
