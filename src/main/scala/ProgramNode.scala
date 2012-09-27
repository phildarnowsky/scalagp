package com.darnowsky.scalagp.ProgramNode

import com.darnowsky.scalagp.ProgramGenerationStrategy.ProgramGenerationStrategy

case class ProgramNode[T](
  val evaluationFunction: (Seq[ProgramNode[T]] => T),
  val childrenCreationStrategy: ProgramGenerationStrategy[T], 
  val depth:Int = 0
) {

  lazy val children: Seq[ProgramNode[T]] = childrenCreationStrategy.generateChildren(depth)

  def evaluate(): T = {
    evaluationFunction(children)
  }
}
