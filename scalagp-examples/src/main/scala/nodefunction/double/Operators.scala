package com.darnowsky.scalagp_examples.nodefunction.double.Operators

import com.darnowsky.scalagp.NodeFunction._
import com.darnowsky.scalagp.ProgramNode.ProgramNode

object Add2Function extends NonterminalNodeFunction[Double](2) {
  def apply(children: Seq[ProgramNode[Double]]) = children.map(_.evaluate).sum

  def toIdentifier = "+"
}
