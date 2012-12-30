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

  def crossoverWith(that: ProgramNode[T]): (ProgramNode[T], ProgramNode[T]) = {
    val thisCrossoverPoint = this.chooseRandomDescendant()
    val thatCrossoverPoint = that.chooseRandomDescendant()

    val thisNewTree = this.insertReplacementSubtree(thatCrossoverPoint, thisCrossoverPoint.pathFromRoot)
    val thatNewTree = that.insertReplacementSubtree(thisCrossoverPoint, thatCrossoverPoint.pathFromRoot)
    (thisNewTree, thatNewTree)
  }

  def toSExpression(): String = {
    if(children.isEmpty)
      evaluationFunction.toIdentifier
    else
      "(" ++ evaluationFunction.toIdentifier ++ children.map(_.toSExpression).fold("")(_ ++ " " ++ _) ++ ")"
  }

  lazy val allDescendants: IndexedSeq[ProgramNode[T]] = children.flatMap(_.allDescendants) :+ this

  def chooseRandomDescendant(): ProgramNode[T] = allDescendants(rng.nextInt(allDescendants.length))

  protected

  lazy val rng = new scala.util.Random

  def insertReplacementSubtree(that: ProgramNode[T], position: Queue[Int]): ProgramNode[T] = {
    val newSubtree = that.updatePathFromRoot(position)
    // todo: map over children applying copyToPosition to them to fix up positions
    if(position.isEmpty) {
      newSubtree
    } else {
      val subtreeIndex = position.last
      val pathToParent = position.init
      val parentToReplace = this.followPath(pathToParent)

      val leftChildren = parentToReplace.children.take(subtreeIndex)
      val rightChildren = parentToReplace.children.drop(subtreeIndex + 1)
      val replacedParent = parentToReplace.copy(children = leftChildren ++ Array(newSubtree) ++ rightChildren)
      insertReplacementSubtree(replacedParent, pathToParent)
    }
  }

  def updatePathFromRoot(newPathFromRoot: Queue[Int]): ProgramNode[T] = {
    val newChildren = children.toIndexedSeq.zipWithIndex.map((nodeTuple) => nodeTuple._1.updatePathFromRoot(newPathFromRoot :+ nodeTuple._2))
    copy(pathFromRoot = newPathFromRoot, children = newChildren) 
  }

  def followPath(path: Queue[Int]): ProgramNode[T] = path.foldLeft(this)((program, index) => program.children(index))
}
