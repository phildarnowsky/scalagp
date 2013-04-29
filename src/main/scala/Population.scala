package com.darnowsky.scalagp.Population

import com.darnowsky.scalagp.ProgramGenerationStrategy.ProgramGenerationStrategy
import com.darnowsky.scalagp.FullGenerationStrategy.FullGenerationStrategy
import com.darnowsky.scalagp.GrowGenerationStrategy.GrowGenerationStrategy
import com.darnowsky.scalagp.ProgramNode.ProgramNode
import com.darnowsky.scalagp.NodeFunction.{NonterminalNodeFunctionCreator, TerminalNodeFunctionCreator}

import scala.collection.immutable.HashMap
import scala.annotation.tailrec

object Population {
  def generate[ProgramType, Double](
    trancheSize: Int, 
    generationStrategies: Seq[ProgramGenerationStrategy[ProgramType]],
    evaluationFunction: ProgramFitnessFunction[ProgramType],
    terminationConditions: List[(Population[_] => Boolean)] = List(),
    reproductionParameters: ReproductionParameters = new ReproductionParameters
  ): Population[ProgramType] = {

    val programs = generationStrategies.flatMap(strategy =>
      (1 to trancheSize).map(_ => strategy.generateProgram))

    new Population(programs, evaluationFunction, terminationConditions, reproductionParameters)
  }

  def terminateOnFitness(maxFitness: Double): (Population[_] => Boolean) = {
    ((population: Population[_]) => population.bestOfRun._2 <= maxFitness)
  }

  def terminateOnGeneration(maxGeneration: Int): (Population[_] => Boolean) = {
    ((population: Population[_]) => population.generation >= maxGeneration)
  }

  def terminateOnFitnessPlateau(plateauGenerations: Int): (Population[_] => Boolean) = {
    ((population: Population[_]) => population.generationsWithoutImprovement >= plateauGenerations)
  }

  // Convenience method to generate population by the popular ramped half-and-
  // half method.
  def generateRampedHalfAndHalf[ProgramType, Double] (
    trancheSize: Int,
    maximumDepth: Int,
    nonterminals: Seq[NonterminalNodeFunctionCreator[ProgramType]],
    terminals: Seq[TerminalNodeFunctionCreator[ProgramType]],
    fitnessFunction: ProgramFitnessFunction[ProgramType],
    terminationConditions: List[(Population[_] => Boolean)] = List(Population.terminateOnFitness(0.0)),
    reproductionParameters: ReproductionParameters = new ReproductionParameters
  ) = {

    val strategies = (1 to maximumDepth).flatMap((depth: Int) => List(
      new FullGenerationStrategy(nonterminals, terminals, depth),
      new GrowGenerationStrategy(nonterminals, terminals, depth)
    ))

    generate(trancheSize, strategies, fitnessFunction, terminationConditions, reproductionParameters)
  }

  def run[ProgramType](initialPopulation: Population[ProgramType], beforeBreedingHook: Traversable[Population[_] => Unit] = None): Population[ProgramType] = {
    var population = initialPopulation

    while(!population.done) {
      beforeBreedingHook.foreach(_(population))
      //println("\n\nPRESS ENTER TO CONTINUE")
      //readLine()
      population = population.breedNewGeneration
    }

    population
  }

  def printGenerationStatistics(population: Population[_]): Unit = {
    println("*******************")
    println("GENERATION " ++ population.generation.toString)
    println("BEST FITNESS OF CURRENT GENERATION: " ++ population.bestOfCurrentGeneration._2.toString)
    println("BEST FITNESS OF RUN: " ++ population.bestOfRun._2.toString)

    //val nodeTotal = population.programs.map(_.allDescendants.length).sum
    //println("TOTAL NODES: " ++ nodeTotal.toString)
    println("CURRENT TIME: " ++ java.util.Calendar.getInstance.getTime.toString)
  }
}

case class ReproductionParameters(
  val depthLimit: Option[Int] = Some(15),
  val crossoverProportion: Double = 0.9,
  val reproductionProportion: Double = 0.1
)

case class PopulationHistory[ProgramType](
  val generation: Int = 1,
  val bestOfPreviousGenerations: Option[(ProgramNode[ProgramType], Double)] = None,
  val previousGenerationsWithoutImprovement: Int = 0
) {

  def updateForNewGeneration(bestOfRun: (ProgramNode[ProgramType], Double), generationsWithoutImprovement: Int) = {
    this.copy(
      bestOfPreviousGenerations =             Some(bestOfRun),
      previousGenerationsWithoutImprovement = generationsWithoutImprovement,
      generation =                            this.generation + 1
    )
  }
}

case class Population[ProgramType](
  val programs: Seq[ProgramNode[ProgramType]], 
  val fitnessFunction: ProgramFitnessFunction[ProgramType],
  val terminationConditions: List[(Population[_] => Boolean)] = List(),
  val reproductionParameters: ReproductionParameters = new ReproductionParameters,
  val history: PopulationHistory[ProgramType] = new PopulationHistory[ProgramType],
  val knownFitnesses: Map[ProgramNode[ProgramType], Double] = new HashMap[ProgramNode[ProgramType], Double]
) {

  lazy val fitnesses = {
    programs.foldLeft(knownFitnesses)((map, program) => map + (program -> fitnessFunction(program.evaluate)))
  }

  lazy val adjustedFitnesses = fitnesses.mapValues((fitness) => 1.0 / (1.0 + fitness))

  lazy val normalizedFitnesses = {
    val fitnessSum = adjustedFitnesses.values.sum
    adjustedFitnesses.mapValues(_ / fitnessSum)
  }

  lazy val normalizedFitnessesInDescendingFitnessOrder = normalizedFitnesses.toList.sortBy(_._2).reverse

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

  def breedNewGeneration: Population[ProgramType] = {
    val programsFromCrossover = breedByCrossover
    val programsAndFitnessesFromReproduction = breedByReproduction
    val programsFromReproduction = programsAndFitnessesFromReproduction.map(_._1)
    val newKnownFitnesses = programsAndFitnessesFromReproduction.toMap

    this.copy(
      programs = (programsFromCrossover ++ programsFromReproduction),
      history = history.updateForNewGeneration(bestOfRun, generationsWithoutImprovement),
      knownFitnesses = newKnownFitnesses
    )
  }

  def generationsWithoutImprovement: Int = {
    if(didBestOfRunImprove)
      0 
    else 
      previousGenerationsWithoutImprovement + 1
  }

  def didBestOfRunImprove(): Boolean = {
    bestOfPreviousGenerations match {
      case None => true
      case Some(oldbest) => (bestOfCurrentGeneration._2 < oldbest._2)
    }
  }

  def breedByCrossover(): Seq[ProgramNode[ProgramType]] = {
    val crossoverCount = (programs.length * crossoverProportion).toInt
    val breeders = List.fill(crossoverCount)(chooseProgramForReproduction())
    val breedingPairs = breeders.grouped(2)

    breedingPairs.flatMap((pair) => pair(0).crossoverWith(pair(1), depthLimit)).toList
  }

  def breedByReproduction(): Seq[(ProgramNode[ProgramType], Double)] = {
    val reproductionCount = (programs.length * reproductionProportion).toInt
    List.fill(reproductionCount){
      val chosenProgram = chooseProgramForReproduction()
      (chosenProgram, fitnesses(chosenProgram))
    }
  }

  def chooseProgramFitnessIndex(): Double = rng.nextDouble()

  def done = this.terminationConditions.exists(_(this))

  // Default toString is so verbose that it can easily provoke an OOME
  override def toString = "Population " + hashCode.toString + " " + "(generation " + generation.toString + ")"

  protected

  // Delegate these values to reproductionParameters
  def depthLimit =             reproductionParameters.depthLimit
  def crossoverProportion =    reproductionParameters.crossoverProportion
  def reproductionProportion = reproductionParameters.reproductionProportion

  // And delegate these values to history
  def generation                            = history.generation
  def bestOfPreviousGenerations             = history.bestOfPreviousGenerations
  def previousGenerationsWithoutImprovement = history.previousGenerationsWithoutImprovement
}

abstract class ProgramFitnessFunction[InputType] extends Function1[InputType, Double] {
}
