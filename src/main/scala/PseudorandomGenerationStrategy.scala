package com.darnowsky.scalagp.PsuedorandomGenerationStrategy

import com.darnowsky.scalagp.ProgramGenerationStrategy._
import com.darnowsky.scalagp.NodeFunction._
import com.darnowsky.scalagp.ProgramNode.ProgramNode

abstract class PseudorandomGenerationStrategy[T](
  nonterminals: Seq[NonterminalNodeFunctionCreator[T]], 
  terminals: Seq[TerminalNodeFunctionCreator[T]],
  depth: Int) 
extends ProgramGenerationStrategy[T](nonterminals, terminals, depth) {
  val rng = new scala.util.Random

  def chooseTerminalFunction(): NodeFunctionCreator[T] = {
    chooseArbitraryNodeFunction(functionsAllowedAtTerminalDepth)
  }

  def chooseNonterminalFunction(): NodeFunctionCreator[T] = {
    chooseArbitraryNodeFunction(functionsAllowedAtNonterminalDepth)
  }

  def functionsAllowedAtTerminalDepth(): Seq[NodeFunctionCreator[T]]
  def functionsAllowedAtNonterminalDepth(): Seq[NodeFunctionCreator[T]]

  protected

  def chooseArbitraryNodeFunction(candidates: Seq[NodeFunctionCreator[T]]): NodeFunctionCreator[T] = candidates(rng.nextInt(candidates.length))
}
