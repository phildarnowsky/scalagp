package com.darnowsky.scalagp.ProgramNode

import com.darnowsky.scalagp.ProgramGenerationStrategy.ProgramGenerationStrategy
import com.darnowsky.scalagp.NodeFunction.NodeFunction
import scala.collection.immutable.Queue

case class ProgramNode[T](
  val evaluationFunction: NodeFunction[T],
  val children: IndexedSeq[ProgramNode[T]],
  val pathFromRoot:Queue[Int] = Queue()
) {

 def this(
    evaluationFunction: NodeFunction[T],
    childrenCreationStrategy: ProgramGenerationStrategy[T], 
    pathFromRoot:Queue[Int]
  ) = {
    this(evaluationFunction, childrenCreationStrategy.generateChildren(evaluationFunction.arity, pathFromRoot), pathFromRoot)
  }

  def evaluate(): T = {
    evaluationFunction(children)
  }

  def crossoverWith(that: ProgramNode[T]): (ProgramNode[T], ProgramNode[T]) = (this, that)

  def toSExpression(): String = {
    if(children.isEmpty)
      evaluationFunction.toIdentifier
    else
      "(" ++ evaluationFunction.toIdentifier ++ children.map(_.toSExpression).fold("")(_ ++ " " ++ _) ++ ")"
  }

  lazy val allDescendants: IndexedSeq[ProgramNode[T]] = children.flatMap(_.allDescendants) :+ this
}
