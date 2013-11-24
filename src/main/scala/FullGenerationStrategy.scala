package com.darnowsky.scalagp.FullGenerationStrategy

import com.darnowsky.scalagp.PsuedorandomGenerationStrategy._
import com.darnowsky.scalagp.NodeFunction._

/** A kind of `ProgramGenerationStrategy` that always generates program trees
    of exactly the given `depth`. */

class FullGenerationStrategy[T](
  nonterminals: Seq[NonterminalNodeFunctionCreator[T]], 
  terminals: Seq[TerminalNodeFunctionCreator[T]],
  depth: Int) 
extends PseudorandomGenerationStrategy[T](nonterminals, terminals, depth) {
  val functionsAllowedAtTerminalDepth = terminals
  val functionsAllowedAtNonterminalDepth = nonterminals

  def successor(): FullGenerationStrategy[T] = new FullGenerationStrategy(nonterminals, terminals, depth - 1)
}
