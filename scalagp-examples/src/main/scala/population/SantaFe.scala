package com.darnowsky.scalagp_examples.population.SantaFe

import com.darnowsky.scalagp.Population._
import com.darnowsky.scalagp_examples.nodefunction.santa_fe.SantaFe._

import scala.annotation.tailrec

class SantaFeFitnessFunction(startingWorld: SantaFeWorld) extends ProgramFitnessFunction[SantaFeFunction] {
  def apply(santaFeFunction: SantaFeFunction) = applyIteration(santaFeFunction, startingWorld)

  @tailrec
  final def applyIteration(santaFeFunction: SantaFeFunction, world: SantaFeWorld): Double = {
    if(world.isEmpty || world.outOfTime)
      world.foodRemaining.toDouble
    else
      applyIteration(santaFeFunction, santaFeFunction(world))
  }
}

object SantaFePopulation {
  def create(trancheSize: Int, maximumDepth: Int) = {
    Population.generateRampedHalfAndHalf(
      trancheSize,
      maximumDepth,
      List(Prog2NodeFunction, Prog3NodeFunction, IfFoodAheadNodeFunction),
      List(MoveNodeFunction, RightNodeFunction, LeftNodeFunction),
      new SantaFeFitnessFunction(SantaFeWorld.defaultTrail),
      List(Population.terminateOnFitness(0.0)),
      Some(15)
    )
  }
}
