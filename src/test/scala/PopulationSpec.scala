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
    "evaluate its constituent programs for fitness, adjusted fitness, and normalized fitness" in {
      val commonChildren = Array(
        new ProgramNode(ConstantEvaluationFunction42, Array():Array[ProgramNode[Int]], Queue(0)),
        new ProgramNode(ConstantEvaluationFunction666, Array():Array[ProgramNode[Int]], Queue(1))
      )

      val testProgram1 = new ProgramNode(HeadEvaluationFunction, commonChildren, Queue())
      val testProgram2 = new ProgramNode(LastEvaluationFunction, commonChildren, Queue())
      val population = new Population(List(testProgram1, testProgram2), TestProgramFitnessFunction)

      // population.fitnesses holds what Koza calls "standardized fitness"
      // (smaller is better)

      val fitnessResult = population.fitnesses
      fitnessResult.size mustEqual 2
      fitnessResult(testProgram1) mustEqual 126.0
      fitnessResult(testProgram2) mustEqual 1998.0

      // adjusted fitness maps fitnesses to [0.0, 1.0] and also exaggerates
      // small differences in fitness, which is generally a win

      val adjustedFitnessResult = population.adjustedFitnesses
      adjustedFitnessResult.size mustEqual 2
      adjustedFitnessResult(testProgram1) mustEqual 0.007874015748031496 // 1.0 / (1.0 + fitness)
      adjustedFitnessResult(testProgram2) mustEqual 5.002501250625312E-4

      // normalized fitness normalized adjusted fitnesses so they sum to 1.0

      val normalizedFitnessResult = population.normalizedFitnesses
      normalizedFitnessResult.size mustEqual 2
      normalizedFitnessResult(testProgram1) mustEqual 0.940263405456256
      normalizedFitnessResult(testProgram2) mustEqual 0.05973659454374412
    }

    "pick a program for reproduction proportionate to its fitness" in {
      val program1 = new ProgramNode(ConstantEvaluationFunction42)
      val program2 = new ProgramNode(ConstantEvaluationFunction1337)
      val program3 = new ProgramNode(ConstantEvaluationFunction3456)

      /* see notes in previous test regarding standardized vs. adjusted vs.
         normalized fitness */

      val epsilon = 0.00000000001

      val population = spy(new Population[Int](List(program1, program2, program3), TestProgramFitnessFunction))

      val program1Cutoff = population.normalizedFitnesses(program1)
      val program2Cutoff = population.normalizedFitnesses(program2)
      val program3Cutoff = population.normalizedFitnesses(program3)

      population.chooseProgramFitnessIndex().returns(0.0).
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

    "create a new generation by crossover with parents chosen by fitness-proportionate selection" in {
      val program1 = spy(new ProgramNode(ConstantEvaluationFunction42))
      val program2 = spy(new ProgramNode(ConstantEvaluationFunction1337))
      val program3 = spy(new ProgramNode(ConstantEvaluationFunction3456))
      val program4 = spy(new ProgramNode(ConstantEvaluationFunction78910))

      val population = spy(new Population[Int](List(program1, program2, program3, program4), TestProgramFitnessFunction, 1.0, 0.0))

      /* Rigging the choice of programs this way involves knowing more about
         the workings of fitness-proportionate selection than I would like,
         but I can't seem to properly stub chooseProgramForReproduction
         itself, possibly because it's overloaded. */

      val program1Index = population.normalizedFitnesses(program1)
      val program2Index = population.normalizedFitnesses(program2)
      val program3Index = population.normalizedFitnesses(program3)
      val epsilon = 0.00000000001

      population.chooseProgramFitnessIndex().returns(program1Index - epsilon).
                                                  thenReturns(program1Index - epsilon).
                                                  thenReturns(program1Index + program2Index + epsilon).
                                                  thenReturns(program1Index - epsilon)
      
      program1.crossoverWith(program1).returns(List(program3, program1))
      program3.crossoverWith(program1).returns(List(program2, program1))

      val newGeneration = population.breedNewGeneration

      newGeneration.programs mustEqual List(program3, program1, program2, program1)
    }

    "create a new generation by reproduction chosen by fitness-proportionate selection" in {
      val program1 = new ProgramNode(ConstantEvaluationFunction42)
      val program2 = new ProgramNode(ConstantEvaluationFunction1337)
      val program3 = new ProgramNode(ConstantEvaluationFunction3456)
      val program4 = new ProgramNode(ConstantEvaluationFunction78910)

      val population = spy(new Population[Int](List(program1, program2, program3, program4), TestProgramFitnessFunction, 0.0, 1.0))

      /* Rigging the choice of programs this way involves knowing more about
         the workings of fitness-proportionate selection than I would like,
         but I can't seem to properly stub chooseProgramForReproduction
         itself, possibly because it's overloaded. */

      val program1Index = population.normalizedFitnesses(program1)
      val program2Index = population.normalizedFitnesses(program2)
      val program3Index = population.normalizedFitnesses(program3)
      val epsilon = 0.00000000001

      population.chooseProgramFitnessIndex().returns(program1Index - epsilon).
                                                  thenReturns(program1Index + program2Index - epsilon).
                                                  thenReturns(program1Index + program2Index + epsilon).
                                                  thenReturns(program1Index - epsilon)

      val newGeneration = population.breedNewGeneration
      newGeneration.programs mustEqual List(program1, program2, program3, program1)
    }

    // TODO: DRY up the previous few tests

    "breed the correct proportions by crossover and reproduction" in {
      val program = spy(new ProgramNode(ConstantEvaluationFunction42))
      val programs = List.fill(100)(program)
      val population = spy(new Population[Int](programs, TestProgramFitnessFunction, 0.7, 0.3))

      val nextGeneration = population.breedNewGeneration

      nextGeneration.programs.length mustEqual 100
      // expected 70 programs generated by crossover, which produces a pair of
      // programs each time
      there were 35.times(program).crossoverWith(program)
    }

    "remember the best-of-run from one generation to the next" in {
      val bestProgram = spy(new ProgramNode(ConstantEvaluationFunction42))
      val goodProgram = spy(new ProgramNode(ConstantEvaluationFunction1337))
      val badProgram = spy(new ProgramNode(ConstantEvaluationFunction3456))
      val worseProgram = spy(new ProgramNode(ConstantEvaluationFunction78910))

      val population = spy(new Population[Int](List(goodProgram, badProgram), TestProgramFitnessFunction, 1.0, 0.0))

      population.bestOfPreviousGenerations mustEqual None
      population.bestOfRun mustEqual (goodProgram, 1337.0 * 3)

      // same stupid trick as above to get goodProgram twice
      population.chooseProgramFitnessIndex().returns(0.0).thenReturns(0.0)
      goodProgram.crossoverWith(goodProgram).returns(List(badProgram, worseProgram))

      val secondGeneration = spy(population.breedNewGeneration)
      secondGeneration.bestOfPreviousGenerations mustEqual Some((goodProgram, 1337.0 * 3))
      secondGeneration.bestOfRun mustEqual (goodProgram, 1337.0 * 3)

      secondGeneration.chooseProgramFitnessIndex().returns(0.0).thenReturns(0.0)
      badProgram.crossoverWith(badProgram).returns(List(badProgram, bestProgram))
      
      val thirdGeneration = secondGeneration.breedNewGeneration
      thirdGeneration.bestOfPreviousGenerations mustEqual Some((goodProgram, 1337.0 * 3))
      thirdGeneration.bestOfRun mustEqual (bestProgram, 42.0 * 3)
    }
  }
}
