package com.darnowsky.scalagp.UniformReproductionChoiceStrategy

import scala.annotation.tailrec

import com.darnowsky.scalagp.ReproductionChoiceStrategy.{ReproductionChoiceStrategy, ReproductionChoiceStrategyGenerator}
import com.darnowsky.scalagp.ProgramNode.ProgramNode

object UniformReproductionChoiceStrategy extends ReproductionChoiceStrategyGenerator {
  def apply[ProgramType](fitnesses: Map[ProgramNode[ProgramType], Double]): UniformReproductionChoiceStrategy[ProgramType] = new UniformReproductionChoiceStrategy(fitnesses)
}

/** A type of `ReproductionChoiceStrategy` where, every time a program is
    requested for reproduction via `chooseProgramForReproduction`, has a
    chance of returning any given program inversely proportional to that
    program's fitness value--remember, smaller fitness value is better, with
    0 indicating a perfect solution, if it's possible for one to exist. */
class UniformReproductionChoiceStrategy[ProgramType](fitnesses: Map[ProgramNode[ProgramType], Double]) extends ReproductionChoiceStrategy[ProgramType](fitnesses) {
  val rng = new scala.util.Random

  def chooseProgramFitnessIndex(): Double = rng.nextDouble()

  def chooseProgramForReproduction(): ProgramNode[ProgramType] = {
    @tailrec
    def chooseProgramForReproductionAcc(index: Double, programsInDescendingFitness: List[(ProgramNode[ProgramType], Double)]): ProgramNode[ProgramType] = {
      val(firstProgram, firstProgramFitness) = programsInDescendingFitness.head
      
      if(index <= firstProgramFitness || programsInDescendingFitness.tail.isEmpty) // the second clause is a little hack to deal with imprecision of double arithmetic
        firstProgram
      else
        chooseProgramForReproductionAcc(index - firstProgramFitness, programsInDescendingFitness.tail)
    }

    chooseProgramForReproductionAcc(
      chooseProgramFitnessIndex(),
      normalizedFitnessesInDescendingFitnessOrder
    )
  }  
  
  lazy val adjustedFitnesses = {
    fitnesses.mapValues((fitness) => 1.0 / (1.0 + fitness))
  }

  lazy val normalizedFitnesses = {
    val fitnessSum = adjustedFitnesses.values.sum
    adjustedFitnesses.mapValues(_ / fitnessSum)
  }

  lazy val normalizedFitnessesInDescendingFitnessOrder = normalizedFitnesses.toList.sortBy(_._2).reverse
}
