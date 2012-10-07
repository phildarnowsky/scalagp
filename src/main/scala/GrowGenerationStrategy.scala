package com.darnowsky.scalagp.GrowGenerationStrategy

import com.darnowsky.scalagp.PsuedorandomGenerationStrategy._
import com.darnowsky.scalagp.NodeFunction.{NodeFunction, TerminalNodeFunction, NonterminalNodeFunction}

class GrowGenerationStrategy[T](nonterminals: Seq[NonterminalNodeFunction[T]], terminals: Seq[TerminalNodeFunction[T]]) extends PseudorandomGenerationStrategy[T](nonterminals, terminals) {
  val functionsAllowedAtTerminalDepth = terminals
  val functionsAllowedAtNonterminalDepth = terminals ++ nonterminals
}

