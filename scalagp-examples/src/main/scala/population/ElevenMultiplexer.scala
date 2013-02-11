package com.darnowsky.scalagp_examples.population.ElevenMultiplexer

import com.darnowsky.scalagp.Population._
import com.darnowsky.scalagp_examples.nodefunction.booleanfunction.ElevenMultiplexer._

class ElevenMultiplexerFitnessCase(caseIndex: Int) {
  val bits = Vector(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024).map{(mask: Int) => (caseIndex & mask) > 0}

  val correctResult = {
    val address = caseIndex & 7
    bits(address + 3)
  }

  def incorrect(booleanFunction: BooleanFunction) = booleanFunction(bits) != correctResult
}

object ElevenMultiplexerFitnessFunction extends ProgramFitnessFunction[BooleanFunction] {
  val fitnessCases = (0 to 2047).map(new ElevenMultiplexerFitnessCase(_))

  def apply(booleanFunction: BooleanFunction) = fitnessCases.count((fitnessCase) => fitnessCase.incorrect(booleanFunction)).toDouble
}

object ElevenMultiplexerPopulation {
  def create(trancheSize: Int, maximumDepth: Int) = {
    Population.generateRampedHalfAndHalf(
      trancheSize,
      maximumDepth,
      List(AndNodeFunction, OrNodeFunction, NotNodeFunction, IfNodeFunction),
      List(Address0NodeFunction, Address1NodeFunction, Address2NodeFunction, Data0NodeFunction, Data1NodeFunction, Data2NodeFunction, Data3NodeFunction, Data4NodeFunction, Data5NodeFunction, Data6NodeFunction, Data7NodeFunction),
      ElevenMultiplexerFitnessFunction
    )
  }
}
