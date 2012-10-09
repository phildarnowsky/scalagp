package com.darnowsky.scalagp_examples.nodefunction.boolean.Operators

import com.darnowsky.scalagp.NodeFunction._
import com.darnowsky.scalagp.ProgramNode.ProgramNode

object OrNodeFunction extends NonterminalNodeFunction[Boolean](2) {
  def apply(children: Seq[ProgramNode[Boolean]]) = {
    children.head.evaluate || children.last.evaluate
  }
}

object AndNodeFunction extends NonterminalNodeFunction[Boolean](2) {
  def apply(children: Seq[ProgramNode[Boolean]]) = {
    children.head.evaluate && children.last.evaluate
  }
}

object NotNodeFunction extends NonterminalNodeFunction[Boolean](1) {
  def apply(children: Seq[ProgramNode[Boolean]]) = {
    !(children.head.evaluate)
  }
}
