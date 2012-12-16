package com.darnowsky.scalagp.GrowGenerationStrategy

import com.darnowsky.scalagp.PsuedorandomGenerationStrategy._
import com.darnowsky.scalagp.NodeFunction._

class GrowGenerationStrategy[T](
  nonterminals: Seq[NonterminalNodeFunctionCreator[T]], 
  terminals: Seq[TerminalNodeFunctionCreator[T]],
  depth: Int) 
extends PseudorandomGenerationStrategy[T](nonterminals, terminals, depth) {
  val functionsAllowedAtTerminalDepth = terminals
  val functionsAllowedAtNonterminalDepth = terminals ++ nonterminals
  def successor(): GrowGenerationStrategy[T] = new GrowGenerationStrategy(nonterminals, terminals, depth - 1)
}

