package com.darnowsky.scalagp.FullGenerationStrategy

import com.darnowsky.scalagp.ProgramGenerationStrategy._
import com.darnowsky.scalagp.NodeFunction.{NodeFunction, TerminalNodeFunction, NonterminalNodeFunction}
import com.darnowsky.scalagp.ProgramNode.ProgramNode

class FullGenerationStrategy[T](nonterminals: Seq[NonterminalNodeFunction[T]], terminals: Seq[TerminalNodeFunction[T]]) extends ProgramGenerationStrategy[T](nonterminals, terminals) {
  val rng = new scala.util.Random

  def generateChildren(depth: Int, arity: Int) = {
    val functionsToChooseFrom = if(isTerminalDepth(depth)) 
      terminals
    else 
      nonterminals

    val evaluationFunctions = Range(0, arity).map(_ => chooseArbitrary(functionsToChooseFrom))
    evaluationFunctions.map(new ProgramNode[T](_, this, depth - 1))
  }

  protected

  def chooseArbitrary(candidates: Seq[NodeFunction[T]]): NodeFunction[T] = candidates(rng.nextInt(candidates.length))
}
