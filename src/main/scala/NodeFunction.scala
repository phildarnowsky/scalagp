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
