package com.darnowsky.scalagp.NodeFunction

import com.darnowsky.scalagp.ProgramNode.ProgramNode

/** A NodeFunction is a function from a `Seq` of 
    [[com.darnowsky.scalagp.ProgramNode.ProgramNode]]`[T]` to just `T`.
    By evaluating these recursively starting from the `ProgramNode` at the head
    of some tree, we can turn an entire such tree into one `T`.

    Typically `T` will itself be a function of some kind, so we have a way to
    go via `NodeFunction`s from an abstract syntax tree of `ProgramNode`s to
    the runnable program represented by that tree.
*/

abstract class NodeFunction[T] extends Function1[Seq[ProgramNode[T]], T] {
  /** The number of arguments this `NodeFunction` takes. */
  val arity: Int

  def toIdentifier(): String
}

/** A `NodeFunction` meant to go only in a leaf node of `ProgramNode`s. */
abstract class TerminalNodeFunction[T] extends NodeFunction[T] with TerminalNodeFunctionCreator[T] {

  /** A leaf node by definition has no children, so this `NodeFunction` takes
      no arguments. 
      
      @return 0 
  */
  val arity = 0

  /** @return this */
  def getNodeFunction = this
}

/** A `NodeFunction` meant to go only in a branch node of `ProgramNode`s. */
abstract class NonterminalNodeFunction[T](val arity: Int) extends NodeFunction[T] with NonterminalNodeFunctionCreator[T] {
  require(arity > 0, "Arity must be positive")

  /** @return this */
  def getNodeFunction = this
}

/** A factory class for `NodeFunction`s. 

    A `NodeFunctionCreator[T]` must implement `getNodeFunction`, which takes
    no arguments and returns a `NodeFunction[T]`.

    For instance, see the ERC `NodeFunctionCreator`s in the examples. An ERC
    is how we get random constants for our programs: when they're instantiated
    they pick a random constant and return that whenever evaluated. In general
    of course two different ERCs will return different random constants. So we
    implement these by having `TerminalNodeCreator`s that return ERC nodes
    with different constants in them.

    Also, a `NodeFunction` implements `NodeFunctionCreator` trivially, by just 
    returning itself.
*/
trait NodeFunctionCreator[T] {
  def getNodeFunction(): NodeFunction[T]
}

trait NonterminalNodeFunctionCreator[T] extends NodeFunctionCreator[T] {
  def getNodeFunction(): NonterminalNodeFunction[T]
}

trait TerminalNodeFunctionCreator[T] extends NodeFunctionCreator[T] {
  def getNodeFunction(): TerminalNodeFunction[T]
}
