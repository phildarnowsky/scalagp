package com.darnowsky.scalagp.ReproductionChoiceStrategy

import com.darnowsky.scalagp.ProgramNode.ProgramNode

trait ReproductionChoiceStrategyGenerator {
  def apply[ProgramType](fitnesses: Map[ProgramNode[ProgramType], Double]): ReproductionChoiceStrategy[ProgramType]
}

abstract class ReproductionChoiceStrategy[ProgramType] (val fitnesses: Map[ProgramNode[ProgramType], Double]) {
  def chooseProgramForReproduction: ProgramNode[ProgramType]
}
