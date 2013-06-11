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

  object AlwaysReproduce extends ReproductionParameters(None, 0.0, 1.0)
  object AlwaysCrossover extends ReproductionParameters(None, 1.0, 0.0)

  "The Population companion object" should {
    "be able to generate a Population via even splits" in {
      val fullStrategy = spy(new FullGenerationStrategy[Int](List(HeadEvaluationFunction, LastEvaluationFunction), List(ConstantEvaluationFunction42, ConstantEvaluationFunction666), 5))
      val growStrategy = spy(new GrowGenerationStrategy[Int](List(HeadEvaluationFunction, LastEvaluationFunction), List(ConstantEvaluationFunction42, ConstantEvaluationFunction666), 5))

      val population = Population.generate(3, List(fullStrategy, growStrategy), TestProgramFitnessFunction)

      there was 3.times(fullStrategy).generateProgram
      there was 3.times(growStrategy).generateProgram
    }

    "terminate on hitting a certain generation count" in {
      val population = Population.generateRampedHalfAndHalf(10, 2, List(HeadEvaluationFunction, LastEvaluationFunction), List(ConstantEvaluationFunction42, ConstantEvaluationFunction666), TestProgramFitnessFunction, List(Population.terminateOnGeneration(20)))
      val finalPopulation = Population.run(population)

      finalPopulation.generation mustEqual 20
    }

    "terminate on hitting a fit enough program" in {
      val badProgram = spy(new ProgramNode(ConstantEvaluationFunction78910))
      val mediocreProgram = spy(new ProgramNode(ConstantEvaluationFunction1337))
      val goodProgram = spy(new ProgramNode(ConstantEvaluationFunction42))

      val population = new Population[Int](List.fill(100)(badProgram), TestProgramFitnessFunction, List(Population.terminateOnFitness(4010.0)), AlwaysCrossover) // just below fitness of mediocreProgram

      badProgram.crossoverWith(badProgram).returns(List.fill(2)(mediocreProgram))
      mediocreProgram.crossoverWith(mediocreProgram).returns(List.fill(2)(goodProgram))

      val result = Population.run(population)
      result.bestOfRun._2 must beLessThanOrEqualTo(4010.0)
      result.generation mustEqual 3
    }

   "terminate on fitness plateauing for too long" in {
      val badProgram = spy(new ProgramNode(ConstantEvaluationFunction78910))
      val mediocreProgram = spy(new ProgramNode(ConstantEvaluationFunction1337))

      val population = new Population[Int](List.fill(100)(badProgram), TestProgramFitnessFunction, List(Population.terminateOnFitnessPlateau(5)), AlwaysCrossover)

      badProgram.crossoverWith(badProgram).returns(List.fill(2)(mediocreProgram))
      mediocreProgram.crossoverWith(mediocreProgram).returns(List.fill(2)(mediocreProgram))

      val result = Population.run(population)
      result.bestOfRun._2 mustEqual 4011.0

      /* Generation 1 improves by default (since there was no previous best)
         Generation 2 improves by breeding mediocreProgram
         Generations 3-7 plateau */
      result.generation mustEqual 7
    }

    "call a hook before breeding a new generation" in {
      class HookTarget {
        def someMethod(): Unit = {
        }
     }

      val testTarget = spy(new HookTarget)
      val pokeHookTarget = ((pop: Population[_]) => testTarget.someMethod)

      val population = Population.generateRampedHalfAndHalf(10, 2, List(HeadEvaluationFunction, LastEvaluationFunction), List(ConstantEvaluationFunction42, ConstantEvaluationFunction666), TestProgramFitnessFunction, List(Population.terminateOnGeneration(2)))

      Population.run(population, Some(pokeHookTarget))
      there was one(testTarget).someMethod()
    }
  }

  "A Population" should {
    "evaluate its constituent programs for fitness" in {
      val commonChildren = Array(
        new ProgramNode(ConstantEvaluationFunction42, Array():Array[ProgramNode[Int]]),
        new ProgramNode(ConstantEvaluationFunction666, Array():Array[ProgramNode[Int]])
      )

      val testProgram1 = new ProgramNode(HeadEvaluationFunction, commonChildren)
      val testProgram2 = new ProgramNode(LastEvaluationFunction, commonChildren)
      val population = new Population(List(testProgram1, testProgram2), TestProgramFitnessFunction)

      // population.fitnesses holds what Koza calls "standardized fitness"
      // (smaller is better)

      val fitnessResult = population.fitnesses
      fitnessResult.size mustEqual 2
      fitnessResult(testProgram1) mustEqual 126.0
      fitnessResult(testProgram2) mustEqual 1998.0
    }

    "create a new generation by crossover with parents chosen by fitness-proportionate selection" in {
      val program1 = spy(new ProgramNode(ConstantEvaluationFunction42))
      val program2 = spy(new ProgramNode(ConstantEvaluationFunction1337))
      val program3 = spy(new ProgramNode(ConstantEvaluationFunction3456))

      val population = spy(new Population[Int](List(program1, program1, program1, program1), TestProgramFitnessFunction, List(), AlwaysCrossover))

      program1.crossoverWith(program1, None).returns(List(program3, program2))

      val newGeneration = population.breedNewGeneration

      newGeneration.programs mustEqual List(program3, program2, program3, program2)
    }

    "create a new generation by reproduction chosen by fitness-proportionate selection" in {
      val program1 = new ProgramNode(ConstantEvaluationFunction42)
      val program2 = new ProgramNode(ConstantEvaluationFunction1337)
      val program3 = new ProgramNode(ConstantEvaluationFunction3456)
      val program4 = new ProgramNode(ConstantEvaluationFunction78910)

      val population = spy(new Population[Int](List(program1, program2, program3, program4), TestProgramFitnessFunction, List(), AlwaysReproduce))
      population.chooseProgramForReproduction.returns(program1).thenReturns(program2).thenReturns(program3).thenReturns(program1)

      val newGeneration = population.breedNewGeneration
      newGeneration.programs mustEqual List(program1, program2, program3, program1)
    }

    // TODO: DRY up the previous few tests

    "breed the correct proportions by crossover and reproduction" in {
      val program = spy(new ProgramNode(ConstantEvaluationFunction42))
      val programs = List.fill(100)(program)
      val population = spy(new Population[Int](programs, TestProgramFitnessFunction, List(), new ReproductionParameters(None, 0.7, 0.3)))

      val nextGeneration = population.breedNewGeneration

      nextGeneration.programs.length mustEqual 100
      // expected 70 programs generated by crossover, which produces a pair of
      // programs each time
      there were 35.times(program).crossoverWith(program, None)
    }

    "remember the best-of-run from one generation to the next" in {
      val bestProgram = spy(new ProgramNode(ConstantEvaluationFunction42))
      val goodProgram = spy(new ProgramNode(ConstantEvaluationFunction1337))
      val badProgram = spy(new ProgramNode(ConstantEvaluationFunction3456))
      val worseProgram = spy(new ProgramNode(ConstantEvaluationFunction78910))

      val population = spy(new Population[Int](List(goodProgram, badProgram), TestProgramFitnessFunction, List(), AlwaysCrossover))

      population.bestOfPreviousGenerations mustEqual None
      population.bestOfRun mustEqual (goodProgram, 1337.0 * 3)

      population.chooseProgramForReproduction.returns(goodProgram).thenReturns(goodProgram)
      goodProgram.crossoverWith(goodProgram).returns(List(badProgram, worseProgram))

      val secondGeneration = spy(population.breedNewGeneration)
      secondGeneration.bestOfPreviousGenerations mustEqual Some((goodProgram, 1337.0 * 3))
      secondGeneration.bestOfRun mustEqual (goodProgram, 1337.0 * 3)

      secondGeneration.chooseProgramForReproduction.returns(badProgram).thenReturns(badProgram)
      badProgram.crossoverWith(badProgram).returns(List(badProgram, bestProgram))
      
      val thirdGeneration = secondGeneration.breedNewGeneration
      thirdGeneration.bestOfPreviousGenerations mustEqual Some((goodProgram, 1337.0 * 3))
      thirdGeneration.bestOfRun mustEqual (bestProgram, 42.0 * 3)
    }
  }
}
