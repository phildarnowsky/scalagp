package com.darnowsky.scalagp.GrowGenerationStrategy

import com.darnowsky.scalagp.PsuedorandomGenerationStrategy._
import com.darnowsky.scalagp.NodeFunction._

class GrowGenerationStrategy[T](
  nonterminals: Seq[NonterminalNodeFunctionCreator[T]], 
  terminals: Seq[TerminalNodeFunctionCreator[T]]) 
extends PseudorandomGenerationStrategy[T](nonterminals, terminals) {
  val functionsAllowedAtTerminalDepth = terminals
  val functionsAllowedAtNonterminalDepth = terminals ++ nonterminals
}

