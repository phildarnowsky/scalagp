package com.darnowsky.scalagp.PsuedorandomGenerationStrategy

import com.darnowsky.scalagp.ProgramGenerationStrategy._
import com.darnowsky.scalagp.NodeFunction.{NodeFunction, TerminalNodeFunction, NonterminalNodeFunction}
import com.darnowsky.scalagp.ProgramNode.ProgramNode

abstract class PseudorandomGenerationStrategy[T](
  nonterminals: Seq[NonterminalNodeFunction[T]], 
  terminals: Seq[TerminalNodeFunction[T]]) 
extends ProgramGenerationStrategy[T](nonterminals, terminals) {
  val rng = new scala.util.Random

  def functionsAllowedAtTerminalDepth(): Seq[NodeFunction[T]]
  def functionsAllowedAtNonterminalDepth(): Seq[NodeFunction[T]]

  def generateChildren(depth: Int, arity: Int) = {
    val functionsToChooseFrom = if(isTerminalDepth(depth)) 
      functionsAllowedAtTerminalDepth
    else 
      functionsAllowedAtNonterminalDepth

    val evaluationFunctions = Range(0, arity).map(_ => chooseArbitraryNodeFunction(functionsToChooseFrom))
    evaluationFunctions.map(new ProgramNode[T](_, this, depth - 1))
  }

  protected

  def chooseArbitraryNodeFunction(candidates: Seq[NodeFunction[T]]): NodeFunction[T] = candidates(rng.nextInt(candidates.length))
}
