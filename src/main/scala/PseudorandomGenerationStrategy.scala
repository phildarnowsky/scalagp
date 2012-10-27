package com.darnowsky.scalagp.PsuedorandomGenerationStrategy

import com.darnowsky.scalagp.ProgramGenerationStrategy._
import com.darnowsky.scalagp.NodeFunction._
import com.darnowsky.scalagp.ProgramNode.ProgramNode

abstract class PseudorandomGenerationStrategy[T](
  nonterminals: Seq[NonterminalNodeFunctionCreator[T]], 
  terminals: Seq[TerminalNodeFunctionCreator[T]]) 
extends ProgramGenerationStrategy[T](nonterminals, terminals) {
  val rng = new scala.util.Random

  def functionsAllowedAtTerminalDepth(): Seq[NodeFunctionCreator[T]]
  def functionsAllowedAtNonterminalDepth(): Seq[NodeFunctionCreator[T]]

  def generateChildren(depth: Int, arity: Int) = {
    val functionsToChooseFrom = if(isTerminalDepth(depth)) 
      functionsAllowedAtTerminalDepth
    else 
      functionsAllowedAtNonterminalDepth

    val evaluationFunctions = Range(0, arity).map(_ => chooseArbitraryNodeFunction(functionsToChooseFrom))
    evaluationFunctions.map(_.getNodeFunction()).map(new ProgramNode[T](_, this, depth - 1))
  }

  protected

  def chooseArbitraryNodeFunction(candidates: Seq[NodeFunctionCreator[T]]): NodeFunctionCreator[T] = candidates(rng.nextInt(candidates.length))
}
