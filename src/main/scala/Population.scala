package com.darnowsky.scalagp.Population

import com.darnowsky.scalagp.ProgramGenerationStrategy.ProgramGenerationStrategy
import com.darnowsky.scalagp.ProgramNode.ProgramNode

object Population {
  def generate[T](
    trancheSize: Int, 
    maximumDepth: Int, 
    generationStrategies: Seq[ProgramGenerationStrategy[T]]): Population[T] = {

    val programs = (1 to maximumDepth).flatMap(depth => 
      generationStrategies.flatMap(strategy =>
        (1 to trancheSize).map(_ => strategy.generateProgram(depth))))

    new Population[T](programs)
  }
}

class Population[T](val programs: Seq[ProgramNode[T]]) {
}
