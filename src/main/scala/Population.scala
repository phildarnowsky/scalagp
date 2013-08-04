package com.darnowsky.scalagp.Population

import com.darnowsky.scalagp.ProgramGenerationStrategy.ProgramGenerationStrategy
import com.darnowsky.scalagp.FullGenerationStrategy.FullGenerationStrategy
import com.darnowsky.scalagp.GrowGenerationStrategy.GrowGenerationStrategy
import com.darnowsky.scalagp.ProgramNode.ProgramNode
import com.darnowsky.scalagp.NodeFunction.{NonterminalNodeFunctionCreator, TerminalNodeFunctionCreator}
import com.darnowsky.scalagp.ReproductionChoiceStrategy.{ReproductionChoiceStrategy, ReproductionChoiceStrategyGenerator}
import com.darnowsky.scalagp.UniformReproductionChoiceStrategy.UniformReproductionChoiceStrategy

import scala.collection.immutable.HashMap
import scala.annotation.tailrec

object Population {
  def generate[ProgramType, Double](
    trancheSize: Int, 
    generationStrategies: Seq[ProgramGenerationStrategy[ProgramType]],
    evaluationFunction: ProgramFitnessFunction[ProgramType],
    terminationConditions: List[(Population[_] => Boolean)] = List(),
    reproductionParameters: ReproductionParameters = new ReproductionParameters,
    reproductionChoiceStrategyGenerator: ReproductionChoiceStrategyGenerator = UniformReproductionChoiceStrategy
  ): Population[ProgramType] = {

    val programs = generationStrategies.flatMap(strategy =>
      (1 to trancheSize).map(_ => strategy.generateProgram))

    new Population(programs, evaluationFunction, terminationConditions, reproductionParameters, reproductionChoiceStrategyGenerator)
  }

  def terminateOnFitness(maxFitness: Double): (Population[_] => Boolean) = {
    ((population: Population[_]) => population.bestOfRun.fitness <= maxFitness)
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
    reproductionParameters: ReproductionParameters = new ReproductionParameters,
    reproductionChoiceStrategyGenerator: ReproductionChoiceStrategyGenerator = UniformReproductionChoiceStrategy
  ) = {

    val strategies = (1 to maximumDepth).flatMap((depth: Int) => List(
      new FullGenerationStrategy(nonterminals, terminals, depth),
      new GrowGenerationStrategy(nonterminals, terminals, depth)
    ))

    generate(trancheSize, strategies, fitnessFunction, terminationConditions, reproductionParameters, reproductionChoiceStrategyGenerator)
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
    println("BEST FITNESS OF CURRENT GENERATION: " ++ population.bestOfCurrentGeneration.fitness.toString)
    println("BEST FITNESS OF RUN: " ++ population.bestOfRun.fitness.toString)

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

class ProgramFitness[ProgramType](val program: ProgramNode[ProgramType], val fitness: Double) {
}

case class PopulationHistory[ProgramType](
  val generation: Int = 1,
  val bestOfPreviousGenerations: Option[(ProgramFitness[ProgramType])] = None,
  val previousGenerationsWithoutImprovement: Int = 0
) {

  def updateForNewGeneration(bestOfRun: ProgramFitness[ProgramType], generationsWithoutImprovement: Int) = {
    this.copy(
      bestOfPreviousGenerations =             Some(bestOfRun),
      previousGenerationsWithoutImprovement = generationsWithoutImprovement,
      generation =                            this.generation + 1
    )
  }
}

/** A collection of all programs in a given generation.

    A `Population[T]` brings together programs of type `T` with a 
    [[com.darnowsky.scalagp.Population.ProgramFitnessFunction]][T]
    and uses the fitness function to evaluate each of the programs, with a
    lower fitness value corresponding to a more fit program.

    One of a `Population`'s main duties is to breed the next generation's
    `Population`, in such a way that fitter programs are likely to reproduce
    more often than less fit programs, and hence pass their adaptive traits to
    the next generation. Our hope is that the selection pressure that the
    fitness function imposes on our programs will, over the course of many
    generations, cause fitness to rise until we get a sufficiently fit program
    (or we hit another termination condition such as an upper limit on total
    generations).

*/

case class Population[ProgramType](
  val programs: Seq[ProgramNode[ProgramType]], 
  val fitnessFunction: ProgramFitnessFunction[ProgramType],
  val terminationConditions: List[(Population[_] => Boolean)] = List(),
  val reproductionParameters: ReproductionParameters = new ReproductionParameters,
  val reproductionChoiceStrategyGenerator: ReproductionChoiceStrategyGenerator = UniformReproductionChoiceStrategy,
  val history: PopulationHistory[ProgramType] = new PopulationHistory[ProgramType],
  val knownFitnesses: Map[ProgramNode[ProgramType], Double] = new HashMap[ProgramNode[ProgramType], Double]
) {

  val fitnesses = {
    programs.foldLeft(knownFitnesses)((map, program) => map + (program -> fitnessFunction(program.evaluate)))
  }

  lazy val bestOfCurrentGeneration = {
    val (a, b) = fitnesses.toList.sortBy(_._2).head
    new ProgramFitness(a,b)
  }

  // is there a more idiomatic way to write this?
  lazy val bestOfRun = bestOfPreviousGenerations match {
                         case None => bestOfCurrentGeneration
                         case Some(oldBest) => List(oldBest, bestOfCurrentGeneration).sortBy(_.fitness).head
                       }

  lazy val reproductionChoiceStrategy = reproductionChoiceStrategyGenerator(fitnesses)

  /** Returns a new Population representing a new generation of programs bred
      from the generation of programs in the Population we call this on. */
  def breedNewGeneration: Population[ProgramType] = {
    val programsFromCrossover = breedByCrossover
    val programFitnessesFromReproduction = breedByReproduction
    val programsFromReproduction = programFitnessesFromReproduction.map(_.program)
    val newKnownFitnesses = programFitnessesFromReproduction.foldLeft(Map.empty[ProgramNode[ProgramType], Double]) ((map, programFitness) => map + (programFitness.program -> programFitness.fitness))

    this.copy(
      programs = (programsFromCrossover ++ programsFromReproduction),
      history = history.updateForNewGeneration(bestOfRun, generationsWithoutImprovement),
      knownFitnesses = newKnownFitnesses
    )
  }

  /** How many generations have we gone without finding a new fittest program.
  */
  def generationsWithoutImprovement: Int = {
    if(didBestOfRunImprove)
      0 
    else 
      previousGenerationsWithoutImprovement + 1
  }

  def didBestOfRunImprove(): Boolean = {
    bestOfPreviousGenerations match {
      case None => true
      case Some(oldbest) => (bestOfCurrentGeneration.fitness < oldbest.fitness)
    }
  }

  def breedByCrossover(): Seq[ProgramNode[ProgramType]] = {
    val crossoverCount = (programs.length * crossoverProportion).toInt
    val breeders = List.fill(crossoverCount)(chooseProgramForReproduction)
    val breedingPairs = breeders.grouped(2)

    breedingPairs.flatMap((pair) => pair(0).crossoverWith(pair(1), depthLimit)).toList
  }

  def breedByReproduction(): Seq[ProgramFitness[ProgramType]] = {
    val reproductionCount = (programs.length * reproductionProportion).toInt
    List.fill(reproductionCount){
      val chosenProgram = chooseProgramForReproduction
      new ProgramFitness(chosenProgram, fitnesses(chosenProgram))
    }
  }

  def done = this.terminationConditions.exists(_(this))

  // Default toString is so verbose that it can easily provoke an OOME
  override def toString = "Population " + hashCode.toString + " " + "(generation " + generation.toString + ")"

  protected

  // Delegate these values to reproductionParameters
  def depthLimit             = reproductionParameters.depthLimit
  def crossoverProportion    = reproductionParameters.crossoverProportion
  def reproductionProportion = reproductionParameters.reproductionProportion

  // And delegate these values to history
  def generation                            = history.generation
  def bestOfPreviousGenerations             = history.bestOfPreviousGenerations
  def previousGenerationsWithoutImprovement = history.previousGenerationsWithoutImprovement

  // And this to reproductionChoiceStrategy
  def chooseProgramForReproduction = reproductionChoiceStrategy.chooseProgramForReproduction
}

abstract class ProgramFitnessFunction[InputType] extends Function1[InputType, Double] {
}
