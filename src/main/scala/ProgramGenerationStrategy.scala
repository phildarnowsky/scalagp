package com.darnowsky.scalagp.ProgramGenerationStrategy

import com.darnowsky.scalagp.ProgramNode.ProgramNode
import com.darnowsky.scalagp.NodeFunction._

abstract class ProgramGenerationStrategy[T] (
  val nonterminals: Seq[NonterminalNodeFunctionCreator[T]], 
  val terminals: Seq[TerminalNodeFunctionCreator[T]]
) {

  def generateChildFunctions(nextLevelTerminal: Boolean, arity: Int): Seq[NodeFunction[T]] = {
    (1 to arity).map(_ => if(nextLevelTerminal) chooseTerminalFunction.getNodeFunction else chooseNonterminalFunction.getNodeFunction)
  }

  def generateProgram(depth: Int): ProgramNode[T] = {
    val evaluationFunction = if(depth == 1)
      chooseTerminalFunction.getNodeFunction
    else
      chooseNonterminalFunction.getNodeFunction

    new ProgramNode[T](
      evaluationFunction,
      this,
      depth,
      None
    )
  }
    //generateChildren(depth, 1, None).head

  protected

  def chooseTerminalFunction(): NodeFunctionCreator[T]
  def chooseNonterminalFunction(): NodeFunctionCreator[T]
}
