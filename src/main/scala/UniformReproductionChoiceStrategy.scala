package com.darnowsky.scalagp.UniformReproductionChoiceStrategy

import scala.annotation.tailrec

import com.darnowsky.scalagp.ReproductionChoiceStrategy.ReproductionChoiceStrategy
import com.darnowsky.scalagp.ProgramNode.ProgramNode

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
  
  lazy val adjustedFitnesses = fitnesses.mapValues((fitness) => 1.0 / (1.0 + fitness))

  lazy val normalizedFitnesses = {
    val fitnessSum = adjustedFitnesses.values.sum
    adjustedFitnesses.mapValues(_ / fitnessSum)
  }

  lazy val normalizedFitnessesInDescendingFitnessOrder = normalizedFitnesses.toList.sortBy(_._2).reverse
}
