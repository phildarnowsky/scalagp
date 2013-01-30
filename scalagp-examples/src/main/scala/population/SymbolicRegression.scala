package com.darnowsky.scalagp_examples.population.SymbolicRegression

import com.darnowsky.scalagp_examples.nodefunction.Polynomial._
import com.darnowsky.scalagp.Population._

class SymbolicRegressionFitnessFunction(val datapoints:Seq[(Double, Double)]) extends ProgramFitnessFunction[Polynomial] {
  def apply(polynomial: Polynomial) = {
    val errors = datapoints.map((datapoint) => {
      val x = datapoint._1
      val y = datapoint._2
      val calculatedY = polynomial(x)
      val difference = y - calculatedY
      scala.math.abs(difference)
    })

    errors.sum
  }
}

// These datapoints were chosen randomly

object RandomRegressionFitnessFunction extends SymbolicRegressionFitnessFunction(List((0.0, 35.48), (1.0, 34.48), (2.0, 3.11), (3.0, 55.35), (4.0, 88.76), (5.0, 55.67), (6.0, 19.85), (7.0, 60.22), (8.0, 15.92), (9.0, 77.71))) {}

// And these are from the gnuplot tutorial "free fall" example:

object FreefallRegressionFitnessFunction extends SymbolicRegressionFitnessFunction(List((0.0, 0.0), (0.5, 1.3), (0.75, 2.8), (1.0, 4.8), (1.25, 7.8)))

object PolynomialPopulation {
  def create(
    trancheSize: Int, 
    maximumDepth: Int, 
    fitnessFunction: SymbolicRegressionFitnessFunction,
    maxFitness: Double): Population[Polynomial] = {
    Population.generateRampedHalfAndHalf(
      trancheSize,
      maximumDepth,
      List(Add, Multiply),
      List(IndependentVariable, new ERCNodeFunctionCreator(-10.0, 10.0)),
      fitnessFunction,
      List(Population.terminateOnFitness(maxFitness), Population.terminateOnGeneration(10000))
    )
  }
}
