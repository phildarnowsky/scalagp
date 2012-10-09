package com.darnowsky.scalagp.ProgramNode

import com.darnowsky.scalagp.ProgramGenerationStrategy.ProgramGenerationStrategy
import com.darnowsky.scalagp.NodeFunction.NodeFunction

case class ProgramNode[T](
  val evaluationFunction: NodeFunction[T],
  val childrenCreationStrategy: ProgramGenerationStrategy[T], 
  val depth:Int = 0
) {

  lazy val children: Seq[ProgramNode[T]] = childrenCreationStrategy.generateChildren(depth, evaluationFunction.arity)

  def evaluate(): T = {
    evaluationFunction(children)
  }
}
