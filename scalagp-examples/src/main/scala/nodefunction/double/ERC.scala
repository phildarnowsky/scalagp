package com.darnowsky.scalagp_examples.nodefunction.double.ERC

import com.darnowsky.scalagp.NodeFunction._
import com.darnowsky.scalagp.ProgramNode.ProgramNode

// An ERC is an Ephemeral Random Constant: a node function that has a constant
// value pseudorandomly chosen at the time of its creation. Actually, since we
// evaluate it lazily, the constant is actually chosen the first time it's
// evaluated

class ERCNodeFunction(lower: Double, upper: Double) extends TerminalNodeFunction[Double] {
  require(upper > lower)

  val spread = upper - lower

  lazy val constantValue = (new scala.util.Random).nextDouble * spread + lower

  def apply(_children: Seq[ProgramNode[Double]]) = constantValue

  def toIdentifier = constantValue.toString
}

class ERCNodeFunctionCreator(lower: Double, upper: Double) extends TerminalNodeFunctionCreator[Double] {
  def getNodeFunction(): ERCNodeFunction = new ERCNodeFunction(lower, upper)
}
