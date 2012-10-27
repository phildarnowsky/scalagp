package com.darnowsky.scalagp.NodeFunction

import com.darnowsky.scalagp.ProgramNode.ProgramNode

abstract class NodeFunction[T] extends Function1[Seq[ProgramNode[T]], T] {
  val arity: Int

  def toIdentifier(): String
}

abstract class TerminalNodeFunction[T] extends NodeFunction[T] with TerminalNodeFunctionCreator[T] {
  val arity = 0

  def getNodeFunction = this
}

abstract class NonterminalNodeFunction[T](val arity: Int) extends NodeFunction[T] with NonterminalNodeFunctionCreator[T] {
  require(arity > 0, "Arity must be positive")
  
  def getNodeFunction = this
}

trait NodeFunctionCreator[T] {
  def getNodeFunction(): NodeFunction[T]
}

trait NonterminalNodeFunctionCreator[T] extends NodeFunctionCreator[T] {
  def getNodeFunction(): NonterminalNodeFunction[T]
}

trait TerminalNodeFunctionCreator[T] extends NodeFunctionCreator[T] {
  def getNodeFunction(): TerminalNodeFunction[T]
}
