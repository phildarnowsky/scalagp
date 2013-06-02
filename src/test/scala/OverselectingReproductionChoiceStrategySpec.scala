import org.specs2.mutable._
import org.specs2.mock._

import com.darnowsky.scalagp.OverselectingReproductionChoiceStrategy._
import com.darnowsky.scalagp.ProgramNode.ProgramNode

class OverselectingReproductionChoiceStrategySpec extends Specification with SpecHelpers with Mockito {
  "An OverselectingReproductionChoiceStrategy" should {
    val eliteFitnesses = List(42, 665)
    val generalFitnesses = List(666, 666, 777, 800, 850, 850, 850)
    // To make it a little more interesting, we'll use some odd numbers.
    // N = 30%, total population size is 9, 0.3 * 9 = 2.7, so we'll get
    // 2 elite programs and 7 general.

    val elitePrograms = intsToProgramFitnessMap(eliteFitnesses)
    val generalPrograms = intsToProgramFitnessMap(generalFitnesses)
    val programs = elitePrograms ++ generalPrograms

    def newTestStrategy = new OverselectingReproductionChoiceStrategy(programs, 0.3, 0.84)
    def strategyOfSize(size: Int) = new OverselectingReproductionChoiceStrategy(intsToProgramFitnessMap(List.fill(size){1}))

   "divide the programs passed it into an elite and a general group, with top N% in the elite" in {
      val testStrategy = newTestStrategy
      // barf. but it works.
      testStrategy.elitePrograms.fitnesses.values.toList.sorted mustEqual eliteFitnesses.map(_.toDouble)
      testStrategy.generalPrograms.fitnesses.values.toList.sorted mustEqual generalFitnesses.map(_.toDouble)
    }

    "choose from the elite group P% of the time, the general the rest of the time" in {
      val testStrategy = spy(newTestStrategy)
      val epsilon = 0.000001

      val eliteLikelihood = 0.84

      testStrategy.chooseSubpopulationIndex.returns(eliteLikelihood - epsilon).thenReturns(eliteLikelihood + epsilon)

      val expectedToBeElite = testStrategy.chooseProgramForReproduction
      val expectedToBeGeneral = testStrategy.chooseProgramForReproduction

      eliteFitnesses must contain(expectedToBeElite.evaluate)
      generalFitnesses must contain(expectedToBeGeneral.evaluate)
    }

    "have at least one program in each group" in {
      val testPrograms = intsToProgramFitnessMap(List.fill(10){5})
      val justTwoTestPrograms = intsToProgramFitnessMap(List(1,2))

      (new OverselectingReproductionChoiceStrategy(testPrograms, 0.05)).elitePrograms.fitnesses.size mustEqual 1
      (new OverselectingReproductionChoiceStrategy(testPrograms, 1.00)).generalPrograms.fitnesses.size mustEqual 1

      (new OverselectingReproductionChoiceStrategy(justTwoTestPrograms, 0.05)).elitePrograms.fitnesses.size mustEqual 1
      (new OverselectingReproductionChoiceStrategy(justTwoTestPrograms, 1.00)).generalPrograms.fitnesses.size mustEqual 1
    }
  }
}
