package com.darnowsky.scalagp.OverselectingReproductionChoiceStrategy

import com.darnowsky.scalagp.ReproductionChoiceStrategy.{ReproductionChoiceStrategy, ReproductionChoiceStrategyGenerator}
import com.darnowsky.scalagp.UniformReproductionChoiceStrategy.UniformReproductionChoiceStrategy
import com.darnowsky.scalagp.ProgramNode.ProgramNode

import scala.collection.immutable.HashMap

object OverselectingReproductionChoiceStrategy extends ReproductionChoiceStrategyGenerator {
  def apply[ProgramType](fitnesses: Map[ProgramNode[ProgramType], Double]): OverselectingReproductionChoiceStrategy[ProgramType] = new OverselectingReproductionChoiceStrategy(fitnesses)
}

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

  protected

  val eliteCount = {
    val rawEliteCount = (fitnesses.size * eliteProportion).floor.toInt
    1.max(rawEliteCount).min(fitnesses.size - 1) // at least 1 in both elite and general populations
  }

  val sortedFitnesses = fitnesses.toList.sortBy(_._2)
  val dividedFitnesses = sortedFitnesses.splitAt(eliteCount)

  val eliteFitnesses = dividedFitnesses._1.toMap[ProgramNode[ProgramType], Double]
  val generalFitnesses = dividedFitnesses._2.toMap[ProgramNode[ProgramType], Double]

  def chooseSubpopulationIndex = rng.nextDouble()
}
