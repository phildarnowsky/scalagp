package com.darnowsky.scalagp_examples.nodefunction.boolean.TrueNodeFunction

import com.darnowsky.scalagp.NodeFunction._
import com.darnowsky.scalagp.ProgramNode.ProgramNode

class TrueNodeFunction extends TerminalNodeFunction[Boolean] {
  def apply(_children: Seq[ProgramNode[Boolean]]) = true
}
