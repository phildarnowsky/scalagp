import scala.collection.immutable.HashMap

import org.specs2.mutable._
import org.specs2.mock._

import com.darnowsky.scalagp.UniformReproductionChoiceStrategy._
import com.darnowsky.scalagp.ProgramNode.ProgramNode

class UniformReproductionChoiceStrategySpec extends Specification with SpecHelpers with Mockito {
  "A UniformReproductionChoiceStrategy" should {
    "evaluate its constituent programs for fitness, adjusted fitness, and normalized fitness" in {

      // adjusted fitness maps fitnesses to [0.0, 1.0] and also exaggerates
      // small differences in fitness, which is generally a win

      // normalized fitness normalizes adjusted fitnesses so they sum to 1.0

      val commonChildren = Array(
        new ProgramNode(ConstantEvaluationFunction42, Array():Array[ProgramNode[Int]], Some(0)),
        new ProgramNode(ConstantEvaluationFunction666, Array():Array[ProgramNode[Int]], Some(1))
      )

      val testProgram1 = new ProgramNode(HeadEvaluationFunction, commonChildren, None)
      val testProgram2 = new ProgramNode(LastEvaluationFunction, commonChildren, None)

      val fitnesses = HashMap(testProgram1 -> 126.0, testProgram2 -> 1998.0)
      val uniformReproductionChoiceStrategy = new UniformReproductionChoiceStrategy(fitnesses)

      val adjustedFitnessResult = uniformReproductionChoiceStrategy.adjustedFitnesses
      adjustedFitnessResult.size mustEqual 2
      adjustedFitnessResult(testProgram1) mustEqual 0.007874015748031496 // 1.0 / (1.0 + fitness)
      adjustedFitnessResult(testProgram2) mustEqual 5.002501250625312E-4

      val normalizedFitnessResult = uniformReproductionChoiceStrategy.normalizedFitnesses
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

      val fitnesses = HashMap(program1 -> 42 * 3.0, program2 -> 1337 * 3.0, program3 -> 3456 * 3.0)

      val uniformReproductionChoiceStrategy = spy(new UniformReproductionChoiceStrategy(fitnesses))

      val program1Cutoff = uniformReproductionChoiceStrategy.normalizedFitnesses(program1)
      val program2Cutoff = uniformReproductionChoiceStrategy.normalizedFitnesses(program2)
      val program3Cutoff = uniformReproductionChoiceStrategy.normalizedFitnesses(program3)

      uniformReproductionChoiceStrategy.chooseProgramFitnessIndex().returns(0.0).
                                                                    thenReturns(program1Cutoff - epsilon).
                                                                    thenReturns(program1Cutoff + epsilon).
                                                                    thenReturns(program1Cutoff + program2Cutoff - epsilon).
                                                                    thenReturns(program1Cutoff + program2Cutoff + epsilon).
                                                                    thenReturns(program1Cutoff + program2Cutoff + program3Cutoff - epsilon)

      uniformReproductionChoiceStrategy.chooseProgramForReproduction mustEqual program1
      uniformReproductionChoiceStrategy.chooseProgramForReproduction mustEqual program1
      uniformReproductionChoiceStrategy.chooseProgramForReproduction mustEqual program2
      uniformReproductionChoiceStrategy.chooseProgramForReproduction mustEqual program2
      uniformReproductionChoiceStrategy.chooseProgramForReproduction mustEqual program3
      uniformReproductionChoiceStrategy.chooseProgramForReproduction mustEqual program3
    }
  }
}
