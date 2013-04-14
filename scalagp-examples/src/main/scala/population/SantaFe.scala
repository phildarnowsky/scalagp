package com.darnowsky.scalagp_examples.population.SantaFe

import com.darnowsky.scalagp.Population._
import com.darnowsky.scalagp_examples.nodefunction.santa_fe.SantaFe._

object SantaFeFitnessFunction extends ProgramFitnessFunction[SantaFeFunction] {
  def apply(santaFeFunction: SantaFeFunction) = throw new UnsupportedOperationException("write me")
}

object SantaFePopulation {
  def create(trancheSize: Int, maximumDepth: Int) = {
    Population.generateRampedHalfAndHalf(
      trancheSize,
      maximumDepth,
      List(Prog2NodeFunction),
      List(MoveNodeFunction, RightNodeFunction, LeftNodeFunction),
      SantaFeFitnessFunction,
      List(Population.terminateOnFitness(0.0)),
      Some(15)
    )
  }
}
