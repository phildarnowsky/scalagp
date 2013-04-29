package com.darnowsky.scalagp_examples.Xor

import com.darnowsky.scalagp.ProgramNode._
import com.darnowsky.scalagp.Population._
import com.darnowsky.scalagp.NodeFunction._

class BooleanPair(override val _1: Boolean, override val _2: Boolean) extends (Boolean, Boolean)(_1, _2)

abstract class BooleanPairFunction extends (BooleanPair => Boolean)

class OrFunction(children: IndexedSeq[BooleanPairFunction]) extends BooleanPairFunction {
  def apply(pair: BooleanPair) = children(0)(pair) || children(1)(pair)
}

class AndFunction(children: IndexedSeq[BooleanPairFunction]) extends BooleanPairFunction {
  def apply(pair: BooleanPair) = children(0)(pair) && children(1)(pair)
}

class NotFunction(children: IndexedSeq[BooleanPairFunction]) extends BooleanPairFunction {
  def apply(pair: BooleanPair) = !(children(0)(pair))
}

object AFunction extends BooleanPairFunction {
  def apply(pair: BooleanPair) = pair._1
}

object BFunction extends BooleanPairFunction {
  def apply(pair: BooleanPair) = pair._2
}

object OrNodeFunction extends NonterminalNodeFunction[BooleanPairFunction](2) {
  val toIdentifier = "OR"
  def apply(children: Seq[ProgramNode[BooleanPairFunction]]): BooleanPairFunction = new OrFunction(children.map(_.evaluate).toIndexedSeq)
}

object AndNodeFunction extends NonterminalNodeFunction[BooleanPairFunction](2) {
  val toIdentifier = "AND"
  def apply(children: Seq[ProgramNode[BooleanPairFunction]]): BooleanPairFunction = new AndFunction(children.map(_.evaluate).toIndexedSeq)
}

object NotNodeFunction extends NonterminalNodeFunction[BooleanPairFunction](1) {
  val toIdentifier = "NOT"
  def apply(children: Seq[ProgramNode[BooleanPairFunction]]): BooleanPairFunction = new NotFunction(children.map(_.evaluate).toIndexedSeq)
}

object ANodeFunction extends TerminalNodeFunction[BooleanPairFunction] {
  val toIdentifier = "A"
  def apply(_children: Seq[ProgramNode[BooleanPairFunction]]) = AFunction
}

object BNodeFunction extends TerminalNodeFunction[BooleanPairFunction] {
  val toIdentifier = "B"
  def apply(_children: Seq[ProgramNode[BooleanPairFunction]]) = BFunction
}

object XorFitnessFunction extends ProgramFitnessFunction[BooleanPairFunction] {
  val testCases = List(
    (new BooleanPair(false, false), false),
    (new BooleanPair(false, true),  true),
    (new BooleanPair(true, false),  true),
    (new BooleanPair(true, true),   false)
  )
  def apply(candidate: BooleanPairFunction) = testCases.count {(testCase) => 
    val pair = testCase._1
    val expectedResult = testCase._2

    candidate(pair) != expectedResult
  }
}

object XorPopulation {
  def create(trancheSize: Int, maximumDepth: Int) = {
    Population.generateRampedHalfAndHalf(
      trancheSize,
      maximumDepth,
      List(OrNodeFunction, AndNodeFunction, NotNodeFunction),
      List(ANodeFunction, BNodeFunction),
      XorFitnessFunction,
      List(Population.terminateOnFitness(0.0))
    )
  }
}

