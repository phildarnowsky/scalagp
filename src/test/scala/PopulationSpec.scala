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
      val program1 = new ProgramNode(ConstantEvaluationFunction42)
      val program2 = new ProgramNode(ConstantEvaluationFunction1337)
      val program3 = new ProgramNode(ConstantEvaluationFunction3456)

      /* The fitnesses we work with are what Koza calls "standardized 
         fitness", a positive real number where a smaller value indicates
         a fitter program. So when we choose proportionate to fitness, we
         actually choose _inversely_ proportionate to standardized fitness. */

      val program1Cutoff = 1.0 / (42 * 3.0)
      val program2Cutoff = 1.0 / (1337 * 3.0)
      val program3Cutoff = 1.0 / (3456 * 3.0)

      val epsilon = 0.00000000001

      val population = spy(new Population[Int](List(program1, program2, program3), TestProgramFitnessFunction))
      population.chooseValueInAllFitnessesRange().returns(0.0).
                                                  thenReturns(program1Cutoff - epsilon).
                                                  thenReturns(program1Cutoff + epsilon).
                                                  thenReturns(program1Cutoff + program2Cutoff - epsilon).
                                                  thenReturns(program1Cutoff + program2Cutoff + epsilon).
                                                  thenReturns(program1Cutoff + program2Cutoff + program3Cutoff - epsilon)

      population.chooseProgramForReproduction mustEqual program1
      population.chooseProgramForReproduction mustEqual program1
      population.chooseProgramForReproduction mustEqual program2
      population.chooseProgramForReproduction mustEqual program2
      population.chooseProgramForReproduction mustEqual program3
      population.chooseProgramForReproduction mustEqual program3
    }

    "create a new generation by fitness-proportionate selection" in {
      val program1 = spy(new ProgramNode(ConstantEvaluationFunction42))
      val program2 = spy(new ProgramNode(ConstantEvaluationFunction1337))
      val program3 = spy(new ProgramNode(ConstantEvaluationFunction3456))
      val program4 = spy(new ProgramNode(ConstantEvaluationFunction78910))

      val population = spy(new Population[Int](List(program1, program2, program3, program4), TestProgramFitnessFunction))

      /* Rigging the choice of programs this way involves knowing more about
         the workings of fitness-proportionate selection than I would like,
         but I can't seem to properly stub chooseProgramForReproduction
         itself, possibly because it's overloaded. */

      val program1Index = 1.0 / (42 * 3.0)
      val program2Index = 1.0 / (1337 * 3.0)
      val program3Index = 1.0 / (3456 * 3.0)
      val epsilon = 0.00000000001

      population.chooseValueInAllFitnessesRange().returns(program1Index - epsilon).
                                                  thenReturns(program1Index - epsilon).
                                                  thenReturns(program1Index + program2Index + epsilon).
                                                  thenReturns(program1Index - epsilon)
      
      program1.crossoverWith(program1).returns(List(program3, program1))
      program3.crossoverWith(program1).returns(List(program2, program1))

      val newGeneration = population.breedNewGeneration

      newGeneration.programs mustEqual List(program3, program1, program2, program1)
    }

    "remember the best-of-run from one generation to the next" in {
      val bestProgram = spy(new ProgramNode(ConstantEvaluationFunction42))
      val goodProgram = spy(new ProgramNode(ConstantEvaluationFunction1337))
      val badProgram = spy(new ProgramNode(ConstantEvaluationFunction3456))
      val worseProgram = spy(new ProgramNode(ConstantEvaluationFunction78910))

      val population = spy(new Population[Int](List(goodProgram, badProgram), TestProgramFitnessFunction))

      population.bestOfPreviousGenerations mustEqual None
      population.bestOfRun mustEqual (goodProgram, 1337.0 * 3)

      // same stupid trick as above to get goodProgram twice
      population.chooseValueInAllFitnessesRange().returns(0.0).thenReturns(0.0)
      goodProgram.crossoverWith(goodProgram).returns(List(badProgram, worseProgram))

      val secondGeneration = spy(population.breedNewGeneration)
      secondGeneration.bestOfPreviousGenerations mustEqual Some((goodProgram, 1337.0 * 3))
      secondGeneration.bestOfRun mustEqual (goodProgram, 1337.0 * 3)

      secondGeneration.chooseValueInAllFitnessesRange().returns(0.0).thenReturns(0.0)
      badProgram.crossoverWith(badProgram).returns(List(badProgram, bestProgram))
      
      val thirdGeneration = secondGeneration.breedNewGeneration
      thirdGeneration.bestOfPreviousGenerations mustEqual Some((goodProgram, 1337.0 * 3))
      thirdGeneration.bestOfRun mustEqual (bestProgram, 42.0 * 3)
    }
  }
}
