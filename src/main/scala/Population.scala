package com.darnowsky.scalagp.Population

import com.darnowsky.scalagp.ProgramGenerationStrategy.ProgramGenerationStrategy
import com.darnowsky.scalagp.FullGenerationStrategy.FullGenerationStrategy
import com.darnowsky.scalagp.GrowGenerationStrategy.GrowGenerationStrategy
import com.darnowsky.scalagp.ProgramNode.ProgramNode
import com.darnowsky.scalagp.NodeFunction.{NonterminalNodeFunctionCreator, TerminalNodeFunctionCreator}

object Population {
  def generate[ProgramType, Double](
    trancheSize: Int, 
    generationStrategies: Seq[ProgramGenerationStrategy[ProgramType]],
    evaluationFunction: ProgramFitnessFunction[ProgramType]
  ): Population[ProgramType] = {

    val programs = generationStrategies.flatMap(strategy =>
      (1 to trancheSize).map(_ => strategy.generateProgram))

    new Population(programs, evaluationFunction)
  }

  // Convenience method to generate population by the popular ramped half-and-
  // half method.
  def generateRampedHalfAndHalf[ProgramType, Double] (
    trancheSize: Int,
    maximumDepth: Int,
    nonterminals: Seq[NonterminalNodeFunctionCreator[ProgramType]],
    terminals: Seq[TerminalNodeFunctionCreator[ProgramType]],
    fitnessFunction: ProgramFitnessFunction[ProgramType]
  ) = {

    val strategies = (1 to maximumDepth).flatMap((depth: Int) => List(
      new FullGenerationStrategy(nonterminals, terminals, depth),
      new GrowGenerationStrategy(nonterminals, terminals, depth)
    ))

    generate(trancheSize, strategies, fitnessFunction)
  }
}

case class Population[ProgramType](
  val programs: Seq[ProgramNode[ProgramType]], 
  val fitnessFunction: ProgramFitnessFunction[ProgramType],
  val bestOfPreviousGenerations: Option[(ProgramNode[ProgramType], Double)] = None
) {

  lazy val fitnesses = {
    val emptyMap = new scala.collection.immutable.HashMap[ProgramNode[ProgramType], Double]
    programs.foldLeft(emptyMap)((map, program) => map + (program -> fitnessFunction(program.evaluate())))
  }

  lazy val bestOfCurrentGeneration = fitnesses.toList.sortBy(_._2).head

  // is there a more idiomatic way to write this?
  lazy val bestOfRun = bestOfPreviousGenerations match {
                         case None => bestOfCurrentGeneration
                         case Some(oldBest) => List(oldBest, bestOfCurrentGeneration).sortBy(_._2).head
                       }

  val rng = new scala.util.Random

  /* Someday, possibly someday soon, we'll have overselection and elitism and
     rank selection and tournament selection and all sorts of fancy selection
     algorithms. When that happens we'll worry about making this more generic.
     Probably use another hierarchy of strategy classes.
  
     But for now, we're just doing good old fitness-proportionate selection,
     just like mother used to make. */

  def chooseProgramForReproduction(): ProgramNode[ProgramType] = {
    /* As noted in PopulationSpec, the fitness values we have are what is
       called "standardized fitness", where a smaller value indicates a more
       fit program--think of it as an error value as compared to a perfect
       solution, which would have a standardized fitness of 0.
       
       So, to do fitness-proportionate selection, we use the reciprocal of the
       fitness value as a normalized fitness value. Division by zero is not a
       problem, since we just get Infinity, which we can work with. */

    chooseProgramForReproduction(
      chooseValueInAllFitnessesRange(), 
      fitnesses.toList.map((fitnessTuple) => (fitnessTuple._1, 1.0 / fitnessTuple._2)).sortBy(_._2).reverse
    )
  }

  def chooseProgramForReproduction(index: Double, programsInDescendingFitness: List[(ProgramNode[ProgramType], Double)]): ProgramNode[ProgramType] = {
    val(firstProgram, firstProgramFitness) = programsInDescendingFitness.head
    
    if(index <= firstProgramFitness || programsInDescendingFitness.tail.isEmpty) // the second clause is a little hack to deal with imprecision of double arithmetic
      firstProgram
    else
      chooseProgramForReproduction(index - firstProgramFitness, programsInDescendingFitness.tail)
  }

  def breedNewGeneration: Population[ProgramType] = {
    val breeders = List.fill(programs.length)(chooseProgramForReproduction())
    val breedingPairs = breeders.grouped(2)

    this.copy(
      programs = breedingPairs.flatMap((pair) => pair(0).crossoverWith(pair(1))).toList,
      bestOfPreviousGenerations = Some(this.bestOfRun)
    )
  }

  def chooseValueInAllFitnessesRange(): Double = fitnesses.map(_._2).sum * rng.nextDouble()
}

abstract class ProgramFitnessFunction[InputType] extends Function1[InputType, Double] {
}
