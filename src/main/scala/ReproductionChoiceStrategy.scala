package com.darnowsky.scalagp.ReproductionChoiceStrategy

import com.darnowsky.scalagp.ProgramNode.ProgramNode

abstract class ReproductionChoiceStrategy[ProgramType] (val fitnesses: Map[ProgramNode[ProgramType], Double]) {
  def chooseProgramForReproduction: ProgramNode[ProgramType]
}


