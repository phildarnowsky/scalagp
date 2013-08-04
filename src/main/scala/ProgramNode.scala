package com.darnowsky.scalagp.ProgramNode

import com.darnowsky.scalagp.ProgramGenerationStrategy.ProgramGenerationStrategy
import com.darnowsky.scalagp.NodeFunction.NodeFunction
import scala.collection.immutable.Queue

/** The building block of the abstract syntax trees that are how we represent
    the programs we are evolving.

    A `ProgramNode[T]` is a wrapper around a 
    [[com.darnowsky.scalagp.NodeFunction.NodeFunction]][T], and an `IndexedSeq`
    of child `ProgramNode`s. When evaluated, this `ProgramNode` returns the 
    result of applying the one to the other.

    `ProgramNode`s can also cross themselves over with other `ProgramNode`s of
    the same type, which is a major way that we hope to breed fitter programs 
    as the generations march on.
*/

case class ProgramNode[T](
  val evaluationFunction: NodeFunction[T],
  val children: IndexedSeq[ProgramNode[T]] = Vector[ProgramNode[T]]()
) {

 def this(
    evaluationFunction: NodeFunction[T],
    childrenCreationStrategy: ProgramGenerationStrategy[T]
  ) = {
    this(evaluationFunction, childrenCreationStrategy.generateChildren(evaluationFunction.arity))
  }

  /** Apply the `evaluationFunction` given at instantiation of this 
      `ProgramNode` to the children of this `ProgramNode`.

      The result is a value of type `T`, which might be a simple constant like
      an `Int`, but which in more interesting cases will itself be a function.
  */

  lazy val evaluate: T = evaluationFunction(children)

  /** Breed two new trees with a partner `ProgramNode`.

      Each of the two parent `ProgramNode`s (`this` and `that`) chooses an
      arbitrary descendant, possibly itself, and the two swap the subtrees
      rooted at those descendants. This yields two children, each of whom
      have a family resemblance to both parents--they share "genes" (i.e.
      subtrees) with the parents.

      If the optional `depthLimit` is given, a child generated this way that's
      deeper than that `depthLimit` isn't used, but rather `this` is returned in 
      its place.
  */

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

  /** A representation of the tree rooted at this `ProgramNode`, as an 
      S-expression.
  */

  def toSExpression(): String = {
    if(children.isEmpty)
      evaluationFunction.toIdentifier
    else
      "(" ++ evaluationFunction.toIdentifier ++ children.map(_.toSExpression).fold("")(_ ++ " " ++ _) ++ ")"
  }

  /** Returns a `Queue` of pairs, where the first element of a pair is a 
      `ProgramNode` that is somewhere in the subtree rooted at this 
      `ProgramNode`, and the second element is the path from this 
      `ProgramNode` to that descendant.

      Note that, in the sequence returned, the root `this` itself will also 
      appear, with an empty `Queue` for the path.

      For example, if `A` is the 2nd child of `B`, and `B` is the 4th child of 
      `C`, then some elements of the return value would be like:

      {{{
        (C, Queue()),
        (B, Queue(4)),
        (A, Queue(4, 2))
      }}}

  */

  def allDescendants(): IndexedSeq[(ProgramNode[T], Queue[Int])] = {
    allDescendantsAcc(Queue())
  }

  /** Choose an arbitrary node out of the subtree rooted at `this`, possibly 
      `this` itself.
  */

  def chooseRandomDescendant(): (ProgramNode[T], Queue[Int]) = allDescendants()((new scala.util.Random).nextInt(allDescendants.length))

  /** Depth of the subtree rooted at this `ProgramNode`. A `ProgramNode` with
      no descendants is considered to have depth 1.
  */

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
