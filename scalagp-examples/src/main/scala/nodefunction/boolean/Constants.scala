package com.darnowsky.scalagp_examples.nodefunction.boolean.Constants

import com.darnowsky.scalagp.NodeFunction._
import com.darnowsky.scalagp.ProgramNode.ProgramNode

object FalseNodeFunction extends TerminalNodeFunction[Boolean] {
  def apply(_children: Seq[ProgramNode[Boolean]]) = false

  override def toIdentifier = "false"
}

object TrueNodeFunction extends TerminalNodeFunction[Boolean] {
  def apply(_children: Seq[ProgramNode[Boolean]]) = true

  override def toIdentifier = "true"
}
