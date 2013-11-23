package com.darnowsky.scalagp.ProgramGenerationStrategy

import com.darnowsky.scalagp.ProgramNode.ProgramNode
import com.darnowsky.scalagp.NodeFunction._
import scala.collection.immutable.Queue

abstract case class ProgramGenerationStrategy[T] (
  val nonterminals: Seq[NonterminalNodeFunctionCreator[T]], 
  val terminals: Seq[TerminalNodeFunctionCreator[T]],
  val depth: Int
) {

  def generateChildFunctions(arity: Int): Seq[NodeFunction[T]] = {
    (1 to arity).map(_ => if(nextLevelTerminal) chooseTerminalFunction.getNodeFunction else chooseNonterminalFunction.getNodeFunction)
  }

  def generateChildren(arity: Int): IndexedSeq[ProgramNode[T]] = {
    val childFunctions = this.generateChildFunctions(arity)

    childFunctions.toIndexedSeq.map{
      (childFunction: NodeFunction[T]) => new ProgramNode[T](
        childFunction, 
        this.successor
      )
    }
  }

  def generateProgram(): ProgramNode[T] = {
    val evaluationFunction = if(depth == 1)
      chooseTerminalFunction.getNodeFunction
    else
      chooseNonterminalFunction.getNodeFunction

    new ProgramNode[T](
      evaluationFunction,
      this
    )
  }

  // The strategy that the children of a node that uses this strategy should
  // be initialized with.
  //
  // Note that for some reason, doubtless the best of such, you can't just
  // make this abstract base class also a case class and use copy here to
  // automatically get a copy of the strategy in question, but with 
  // depth = depth - 1. I sort of hate that.
  def successor(): ProgramGenerationStrategy[T]

  protected def chooseTerminalFunction(): NodeFunctionCreator[T]
  protected def chooseNonterminalFunction(): NodeFunctionCreator[T]

  protected def nextLevelTerminal(): Boolean = (depth - 1 == 1)
}
