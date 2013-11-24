package com.darnowsky.scalagp.GrowGenerationStrategy

import com.darnowsky.scalagp.PsuedorandomGenerationStrategy._
import com.darnowsky.scalagp.NodeFunction._

/** A kind of `ProgramGenerationStrategy` that always generates program trees
    of less than or equal to the given `depth`. */

class GrowGenerationStrategy[T](
  nonterminals: Seq[NonterminalNodeFunctionCreator[T]], 
  terminals: Seq[TerminalNodeFunctionCreator[T]],
  depth: Int) 
extends PseudorandomGenerationStrategy[T](nonterminals, terminals, depth) {
  val functionsAllowedAtTerminalDepth = terminals
  val functionsAllowedAtNonterminalDepth = terminals ++ nonterminals
  def successor(): GrowGenerationStrategy[T] = new GrowGenerationStrategy(nonterminals, terminals, depth - 1)
}

