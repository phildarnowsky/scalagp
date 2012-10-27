package com.darnowsky.scalagp.ProgramGenerationStrategy

import com.darnowsky.scalagp.ProgramNode.ProgramNode
import com.darnowsky.scalagp.NodeFunction._

abstract class ProgramGenerationStrategy[T] (
  val nonterminals: Seq[NonterminalNodeFunctionCreator[T]], 
  val terminals: Seq[TerminalNodeFunctionCreator[T]]) {
  def generateChildren(depth: Int, arity: Int): Seq[ProgramNode[T]]
  def generateProgram(depth: Int): ProgramNode[T] = generateChildren(depth, 1).head

  protected

  def isTerminalDepth(depth: Int) = depth == 1
}
