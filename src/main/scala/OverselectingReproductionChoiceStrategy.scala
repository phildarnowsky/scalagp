package com.darnowsky.scalagp.OverselectingReproductionChoiceStrategy

import com.darnowsky.scalagp.ReproductionChoiceStrategy.{ReproductionChoiceStrategy, ReproductionChoiceStrategyGenerator}
import com.darnowsky.scalagp.UniformReproductionChoiceStrategy.UniformReproductionChoiceStrategy
import com.darnowsky.scalagp.ProgramNode.ProgramNode

import scala.collection.immutable.HashMap

object OverselectingReproductionChoiceStrategy extends ReproductionChoiceStrategyGenerator {
  def apply[ProgramType](fitnesses: Map[ProgramNode[ProgramType], Double]): OverselectingReproductionChoiceStrategy[ProgramType] = new OverselectingReproductionChoiceStrategy(fitnesses)
}

/** A type of `ReproductionChoiceStrategy` that separates programs, based on 
    their fitness values, into an elite subpopulation and a general 
    subpopulation, and where `chooseProgramForReproduction` is more likely to
    return a program from the elite group. 
   
    `eliteProportion` and `eliteLikelihood` should both be strictly between 
    0.0 and 1.0, representing the proportion of programs that will be put in
    the elite group, and the likelihood that a program will be chosen from
    the elite group (as opposed to the general group) on a particular call to
    `chooseProgramForReproduction`.  */

class OverselectingReproductionChoiceStrategy[ProgramType](fitnesses: Map[ProgramNode[ProgramType], Double], eliteProportion: Double = 0.32, eliteLikelihood: Double = 0.8) extends ReproductionChoiceStrategy[ProgramType](fitnesses) { 
  val rng = new scala.util.Random

  def chooseProgramForReproduction(): ProgramNode[ProgramType] = {
    if(chooseSubpopulationIndex < eliteLikelihood) {
      elitePrograms.chooseProgramForReproduction()
    } else {
      generalPrograms.chooseProgramForReproduction()
    }
  }

  lazy val elitePrograms = new UniformReproductionChoiceStrategy(eliteFitnesses)

  lazy val generalPrograms = new UniformReproductionChoiceStrategy(generalFitnesses)

  def chooseSubpopulationIndex = rng.nextDouble()

  protected val eliteCount = {
    val rawEliteCount = (fitnesses.size * eliteProportion).floor.toInt
    1.max(rawEliteCount).min(fitnesses.size - 1) // at least 1 in both elite and general populations
  }

  protected val sortedFitnesses = fitnesses.toList.sortBy(_._2)
  protected val dividedFitnesses = sortedFitnesses.splitAt(eliteCount)

  protected val eliteFitnesses = dividedFitnesses._1.toMap[ProgramNode[ProgramType], Double]
  protected val generalFitnesses = dividedFitnesses._2.toMap[ProgramNode[ProgramType], Double]
}
