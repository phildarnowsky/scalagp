package com.darnowsky.scalagp.NodeFunction

import com.darnowsky.scalagp.ProgramNode.ProgramNode

abstract class NodeFunction[T] extends Function1[Seq[ProgramNode[T]], T] {
  val arity: Int
}

abstract class TerminalNodeFunction[T] extends NodeFunction[T] {
  val arity = 0
}

abstract class NonterminalNodeFunction[T](val arity: Int) extends NodeFunction[T] {
  require(arity > 0, "Arity must be positive")
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

object NodeFunctionCreatorImplicits {
  implicit def nonTerminalNodeFunctionCreator2NonTerminalNode[T](creator: NonterminalNodeFunctionCreator[T]): NonterminalNodeFunction[T] = creator.getNodeFunction()
  implicit def terminalNodeFunctionCreator2TerminalNode[T](creator: TerminalNodeFunctionCreator[T]): TerminalNodeFunction[T] = creator.getNodeFunction()
}
