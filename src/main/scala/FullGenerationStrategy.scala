package com.darnowsky.scalagp.FullGenerationStrategy

import com.darnowsky.scalagp.PsuedorandomGenerationStrategy._
import com.darnowsky.scalagp.NodeFunction._

class FullGenerationStrategy[T](
  nonterminals: Seq[NonterminalNodeFunctionCreator[T]], 
  terminals: Seq[TerminalNodeFunctionCreator[T]]) 
extends PseudorandomGenerationStrategy[T](nonterminals, terminals) {
  val functionsAllowedAtTerminalDepth = terminals
  val functionsAllowedAtNonterminalDepth = nonterminals
}
