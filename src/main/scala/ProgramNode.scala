package com.darnowsky.scalagp.ProgramNode

import com.darnowsky.scalagp.ProgramGenerationStrategy.ProgramGenerationStrategy
import com.darnowsky.scalagp.NodeFunction.NodeFunction
import scala.collection.immutable.Queue

case class ProgramNode[T](
  val evaluationFunction: NodeFunction[T],
  val children: IndexedSeq[ProgramNode[T]] = Vector[ProgramNode[T]](),
  val parentIndex: Option[Int] = None
) {

 def this(
    evaluationFunction: NodeFunction[T],
    childrenCreationStrategy: ProgramGenerationStrategy[T],
    parentIndex: Option[Int]
  ) = {
    this(evaluationFunction, childrenCreationStrategy.generateChildren(evaluationFunction.arity), parentIndex)
  }

  lazy val evaluate: T = evaluationFunction(children)

  def crossoverWith(that: ProgramNode[T], depthLimit: Option[Int] = None): Seq[ProgramNode[T]] = {
    val (thisCrossoverNode, thisCrossoverPath) = this.chooseRandomDescendant()
    val (thatCrossoverNode, thatCrossoverPath) = that.chooseRandomDescendant()

    val thisNewTree = this.insertReplacementSubtree(thatCrossoverNode, thisCrossoverPath)
    val thatNewTree = that.insertReplacementSubtree(thisCrossoverNode, thatCrossoverPath)

    depthLimit match {
      case None    => List(thisNewTree, thatNewTree)
      case Some(x) => {
        // enforce depth limit
        val clippedThisNewTree = if (thisNewTree.depth > x) this else thisNewTree
        val clippedThatNewTree = if (thatNewTree.depth > x) this else thatNewTree
        List(clippedThisNewTree, clippedThatNewTree)
      }
    }
  }

  def toSExpression(): String = {
    if(children.isEmpty)
      evaluationFunction.toIdentifier
    else
      "(" ++ evaluationFunction.toIdentifier ++ children.map(_.toSExpression).fold("")(_ ++ " " ++ _) ++ ")"
  }

  def allDescendants(): IndexedSeq[(ProgramNode[T], Queue[Int])] = {
    allDescendantsAcc(Queue())
  }

  def chooseRandomDescendant(): (ProgramNode[T], Queue[Int]) = allDescendants()((new scala.util.Random).nextInt(allDescendants.length))

  lazy val depth: Int = if (children.isEmpty)
                          1
                        else
                          1 + children.map(_.depth).max

  protected

  def allDescendantsAcc(queueToHere: Queue[Int]): IndexedSeq[(ProgramNode[T], Queue[Int])] = {
    children.zipWithIndex.flatMap((childTuple) => childTuple._1.allDescendantsAcc(queueToHere :+ childTuple._2)) :+
                                 (this, queueToHere)
                         
  }


  def insertReplacementSubtree(that: ProgramNode[T], position: Queue[Int]): ProgramNode[T] = {
    val newSubtree = that
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

  def followPath(path: Queue[Int]): ProgramNode[T] = path.foldLeft(this)((program, index) => program.children(index))
}
