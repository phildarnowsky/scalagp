package com.darnowsky.scalagp.Population

import com.darnowsky.scalagp.ProgramGenerationStrategy.ProgramGenerationStrategy
import com.darnowsky.scalagp.FullGenerationStrategy.FullGenerationStrategy
import com.darnowsky.scalagp.GrowGenerationStrategy.GrowGenerationStrategy
import com.darnowsky.scalagp.ProgramNode.ProgramNode
import com.darnowsky.scalagp.NodeFunction.{NonterminalNodeFunctionCreator, TerminalNodeFunctionCreator}

object Population {
  def generate[ProgramType, FitnessType](
    trancheSize: Int, 
    maximumDepth: Int, 
    generationStrategies: Seq[ProgramGenerationStrategy[ProgramType]],
    evaluationFunction: ProgramFitnessFunction[ProgramType, FitnessType]
  ): Population[ProgramType, FitnessType] = {

    val programs = (1 to maximumDepth).flatMap(depth => 
      generationStrategies.flatMap(strategy =>
        (1 to trancheSize).map(_ => strategy.generateProgram(depth))))

    new Population(programs, evaluationFunction)
  }

  // Convenience method to generate population by the popular ramped half-and-
  // half method.
  def generateRampedHalfAndHalf[ProgramType, FitnessType] (
    trancheSize: Int,
    maximumDepth: Int,
    nonterminals: Seq[NonterminalNodeFunctionCreator[ProgramType]],
    terminals: Seq[TerminalNodeFunctionCreator[ProgramType]],
    evaluationFunction: ProgramFitnessFunction[ProgramType, FitnessType]
  ) = {
    val strategies = List(
      new FullGenerationStrategy(nonterminals, terminals),
      new GrowGenerationStrategy(nonterminals, terminals)
    )

    generate(trancheSize, maximumDepth, strategies, evaluationFunction)
  }
}

case class Population[ProgramType, FitnessType](
  val programs: Seq[ProgramNode[ProgramType]], 
  val fitnessFunction: ProgramFitnessFunction[ProgramType, FitnessType]
) {
  def fitnesses(): Map[ProgramNode[ProgramType], FitnessType] = {
    val emptyMap = new scala.collection.immutable.HashMap[ProgramNode[ProgramType], FitnessType]
    programs.foldLeft(emptyMap)((map, program) => map + (program -> fitnessFunction(program.evaluate())))
  }
}

abstract class ProgramFitnessFunction[InputType, OutputType] extends Function1[InputType, OutputType] {
}
