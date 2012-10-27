package com.darnowsky.scalagp.Population

import com.darnowsky.scalagp.ProgramGenerationStrategy.ProgramGenerationStrategy
import com.darnowsky.scalagp.FullGenerationStrategy.FullGenerationStrategy
import com.darnowsky.scalagp.GrowGenerationStrategy.GrowGenerationStrategy
import com.darnowsky.scalagp.ProgramNode.ProgramNode
import com.darnowsky.scalagp.NodeFunction.{NonterminalNodeFunctionCreator, TerminalNodeFunctionCreator}

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

  // Convenience method to generate population by the popular ramped half-and-
  // half method.
  def generateRampedHalfAndHalf[T](
    trancheSize: Int,
    maximumDepth: Int,
    nonterminals: Seq[NonterminalNodeFunctionCreator[T]],
    terminals: Seq[TerminalNodeFunctionCreator[T]]
  ) ={
    val strategies = List(
      new FullGenerationStrategy(nonterminals, terminals),
      new GrowGenerationStrategy(nonterminals, terminals)
    )

    generate(trancheSize, maximumDepth, strategies)
  }
}

class Population[T](val programs: Seq[ProgramNode[T]]) {
}
