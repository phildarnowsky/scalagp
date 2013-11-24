package com.darnowsky.scalagp.ProgramGenerationStrategy

import com.darnowsky.scalagp.ProgramNode.ProgramNode
import com.darnowsky.scalagp.NodeFunction._
import scala.collection.immutable.Queue

/** A ProgramGenerationStrategy generates the programs that make up our
    populations of candidate solutions. Each program is implemented as a tree 
    of `ProgramNode`s.
    
    A subclass of ProgramGenerationStrategy must implement `chooseTerminalFunction` 
    and `chooseNonterminalFunction`, which are used to choose a `NodeFunction`, 
    at terminal and nonterminal depth respectively, with which to initialize
    a `ProgramNode`.
    
    Additionally it must implement `successor`. `successor` is used when 
    creating the children of a given `ProgramNode`. `successor` is the 
    ProgramGenerationStrategy that those children will be initialized with.
*/

abstract case class ProgramGenerationStrategy[T] (
  val nonterminals: Seq[NonterminalNodeFunctionCreator[T]], 
  val terminals: Seq[TerminalNodeFunctionCreator[T]],
  val depth: Int
) {

  def generateChildFunctions(arity: Int): Seq[NodeFunction[T]] = {
    (1 to arity).map(_ => if(nextLevelTerminal) chooseTerminalFunction.getNodeFunction else chooseNonterminalFunction.getNodeFunction)
  }

/** Generate a list of `ProgramNode`s of length `arity` */
  def generateChildren(arity: Int): IndexedSeq[ProgramNode[T]] = {
    val childFunctions = this.generateChildFunctions(arity)

    childFunctions.toIndexedSeq.map{
      (childFunction: NodeFunction[T]) => new ProgramNode[T](
        childFunction, 
        this.successor
      )
    }
  }

  /** Create a single program, consisting of a tree of `ProgramNode`s. This 
      uses the `depth` instance variable to guide how deep the program created
      will be, though note that different subclasses can interpret `depth` in
      different ways--as the exact depth of the tree to be created, as the
      maximum depth, and so on. */
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

  /** The strategy that the children of a node that uses this strategy should
      be initialized with. */
  def successor(): ProgramGenerationStrategy[T]

  protected def chooseTerminalFunction(): NodeFunctionCreator[T]
  protected def chooseNonterminalFunction(): NodeFunctionCreator[T]

  protected def nextLevelTerminal(): Boolean = (depth - 1 == 1)
}
