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

// And these are from the function y = x^2 + 3.14x - 2.718

object ArbitraryQuadraticFitnessFunction extends SymbolicRegressionFitnessFunction(List( (0.0,-2.718), (0.5,-0.898), (1.0,1.422), (1.5,4.242), (2.0,7.562), (2.5,11.382), (3.0,15.702), (3.5,20.522), (4.0,25.842), (4.5,31.662), (5.0,37.982), (5.5,44.802), (6.0,52.122), (6.5,59.942), (7.0,68.262), (7.5,77.082), (8.0,86.402), (8.5,96.222), (9.0,106.542), (9.5,117.362), (10.0,128.682), (10.5,140.502), (11.0,152.822), (11.5,165.642), (12.0,178.962), (12.5,192.782), (13.0,207.102), (13.5,221.922), (14.0,237.242), (14.5,253.062), (15.0,269.382), (15.5,286.202), (16.0,303.522), (16.5,321.342), (17.0,339.662), (17.5,358.482), (18.0,377.802), (18.5,397.622), (19.0,417.942), (19.5,438.762), (20.0,460.082)))

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
      List(Population.terminateOnFitness(maxFitness), Population.terminateOnGeneration(10000), Population.terminateOnFitnessPlateau(50))
    )
  }
}
