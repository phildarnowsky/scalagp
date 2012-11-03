package com.darnowsky.scalagp_examples.population.FindRootPopulation

import com.darnowsky.scalagp_examples.nodefunction.double.Operators._
import com.darnowsky.scalagp_examples.nodefunction.double.ERC._
import com.darnowsky.scalagp.Population._

object RootFinderFitnessFunction extends ProgramFitnessFunction[Double, Double] {
  // Find root of X^4 - 2X^2 (roots are 0, +/- sqrt 2)
  // Fitness function is magnitude of X^4 - 2X^2, that is, the error if we
  // take X as a root.
  def apply(x: Double) = scala.math.pow(x, 4.0) - 2.0 * scala.math.pow(x, 2.0)
}

object FindRootPopulation {
  def create(trancheSize: Int, maximumDepth: Int): Population[Double, Double] = {
    Population.generateRampedHalfAndHalf(
      trancheSize, 
      maximumDepth,
      List(AddFunction, SubtractFunction, MultiplyFunction, ProtectedDivideFunction),
      List(new ERCNodeFunctionCreator(-10.0, 10.0)),
      RootFinderFitnessFunction
    )}
}
