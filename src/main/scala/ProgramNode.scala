package com.darnowsky.scalagp.ProgramNode

import com.darnowsky.scalagp.ProgramGenerationStrategy.ProgramGenerationStrategy
import com.darnowsky.scalagp.NodeFunction.NodeFunction

class ProgramNode[T](
  val evaluationFunction: NodeFunction[T],
  val childrenCreationStrategy: ProgramGenerationStrategy[T], 
  val depth:Int = 0,
  val parent: Option[ProgramNode[T]],
  val indexWithinParent: Int = 0
) {

  lazy val children: IndexedSeq[ProgramNode[T]] = {
    val childFunctions = childrenCreationStrategy.generateChildFunctions(nextLevelTerminal, evaluationFunction.arity)
    childFunctions.toIndexedSeq.zipWithIndex.map{
      (functionTuple) => new ProgramNode[T](
        functionTuple._1, 
        childrenCreationStrategy, 
        depth - 1, 
        Some(this), 
        functionTuple._2)
    }
  }

  val nextLevelTerminal = (this.depth - 1 == 1)

  def evaluate(): T = {
    evaluationFunction(children)
  }

  def toSExpression(): String = {
    if(children.isEmpty)
      evaluationFunction.toIdentifier
    else
      "(" ++ evaluationFunction.toIdentifier ++ children.map(_.toSExpression).fold("")(_ ++ " " ++ _) ++ ")"
  }

  def pathToRoot(soFar: List[Int] = List()): List[Int] = {
    parent match {
      case None => soFar.reverse
      case Some(parentNode) => parentNode.pathToRoot(indexWithinParent :: soFar)
    }
  }
}
