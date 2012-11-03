package com.darnowsky.scalagp_examples.nodefunction.double.Operators

import com.darnowsky.scalagp.NodeFunction._
import com.darnowsky.scalagp.ProgramNode.ProgramNode

abstract class BinaryOperator extends NonterminalNodeFunction[Double](2) {
}

object AddFunction extends BinaryOperator {
  def apply(children: Seq[ProgramNode[Double]]) = children.map(_.evaluate()).sum
  def toIdentifier = "+"
}

object SubtractFunction extends BinaryOperator {
  def apply(children: Seq[ProgramNode[Double]]) = children(0).evaluate() - children(1).evaluate
  def toIdentifier = "-"
}

object MultiplyFunction extends BinaryOperator {
  def apply(children: Seq[ProgramNode[Double]]) = children.map(_.evaluate()).product
  def toIdentifier = "*"
}

object ProtectedDivideFunction extends BinaryOperator {
  def apply(children: Seq[ProgramNode[Double]]) = {
    children(1).evaluate() match {
      case 0.0 => 0.0
      case denominator => children(0).evaluate() / denominator
    }
  }
  def toIdentifier = "/"
}
