package com.darnowsky.scalagp.ProgramGenerationStrategy

import com.darnowsky.scalagp.ProgramNode.ProgramNode
import com.darnowsky.scalagp.NodeFunction.{NodeFunction, TerminalNodeFunction, NonterminalNodeFunction}

abstract class ProgramGenerationStrategy[T](
  val nonterminals: Seq[NonterminalNodeFunction[T]], 
  val terminals: Seq[TerminalNodeFunction[T]]) {
  def generateChildren(depth: Int, arity: Int): Seq[ProgramNode[T]]

  protected

  def isTerminalDepth(depth: Int) = depth == 1
}
