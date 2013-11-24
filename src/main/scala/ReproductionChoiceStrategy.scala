package com.darnowsky.scalagp.ReproductionChoiceStrategy

import com.darnowsky.scalagp.ProgramNode.ProgramNode

/** Trait convenient for creating factory objects for 
    `ReproductionChoiceStrategy`, which we can then pass around. */

trait ReproductionChoiceStrategyGenerator {
  def apply[ProgramType](fitnesses: Map[ProgramNode[ProgramType], Double]): ReproductionChoiceStrategy[ProgramType]
}

/** Takes a `Map` of `ProgramNode`s to `Double`s, those `Double`s being the 
    fitnesses of the corresponding programs, and via 
    `chooseProgramForReproduction` chooses programs on request to reproduce
    into the next generation. */

abstract class ReproductionChoiceStrategy[ProgramType] (val fitnesses: Map[ProgramNode[ProgramType], Double]) {
  def chooseProgramForReproduction: ProgramNode[ProgramType]
}
