package com.darnowsky.scalagp.ProgramNode

import com.darnowsky.scalagp.ProgramGenerationStrategy.ProgramGenerationStrategy
import com.darnowsky.scalagp.NodeFunction.NodeFunction

class ProgramNode[T](
  val evaluationFunction: NodeFunction[T],
  val childrenCreationStrategy: ProgramGenerationStrategy[T], 
  val depth:Int = 0
) {

  lazy val children: Seq[ProgramNode[T]] = childrenCreationStrategy.generateChildren(depth, evaluationFunction.arity)

  def evaluate(): T = {
    evaluationFunction(children)
  }

  def toSExpression(): String = {
    if(children.isEmpty)
      evaluationFunction.toIdentifier
    else
      "(" ++ evaluationFunction.toIdentifier ++ children.map(_.toSExpression).fold("")(_ ++ " " ++ _) ++ ")"
  }
}
