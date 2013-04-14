package com.darnowsky.scalagp_examples.nodefunction.santa_fe.SantaFe

import com.darnowsky.scalagp.NodeFunction._
import com.darnowsky.scalagp.ProgramNode.ProgramNode

class SantaFeWorld {
  def moveForward(): SantaFeWorld = {
    throw new UnsupportedOperationException("write me")
  }

  def turnRight(): SantaFeWorld = {
    throw new UnsupportedOperationException("write me")
  }

  def turnLeft(): SantaFeWorld = {
    throw new UnsupportedOperationException("write me")
  }

  def foodAhead(): Boolean = {
    throw new UnsupportedOperationException("write me")
  }
}

abstract class SantaFeFunction extends Function1[SantaFeWorld, SantaFeWorld] 

object MoveAction extends SantaFeFunction {
  def apply(world:SantaFeWorld) = world.moveForward()
}

object MoveNodeFunction extends TerminalNodeFunction[SantaFeFunction] {
  def toIdentifier = "(MOVE)"
  def apply(_children: Seq[ProgramNode[SantaFeFunction]]) = MoveAction
}

object RightAction extends SantaFeFunction {
  def apply(world:SantaFeWorld) = world.turnRight()
}

object RightNodeFunction extends TerminalNodeFunction[SantaFeFunction] {
  def toIdentifier = "(RIGHT)"
  def apply(_children: Seq[ProgramNode[SantaFeFunction]]) = RightAction
}

object LeftAction extends SantaFeFunction {
  def apply(world:SantaFeWorld) = world.turnLeft()
}

object LeftNodeFunction extends TerminalNodeFunction[SantaFeFunction] {
  def toIdentifier = "(LEFT)"
  def apply(_children: Seq[ProgramNode[SantaFeFunction]]) = LeftAction
}

class ProgN(childFunctions: Seq[SantaFeFunction]) extends SantaFeFunction {
  def apply(world: SantaFeWorld) = (world /: childFunctions)((world, childFunction) => childFunction(world))
}

object Prog2NodeFunction extends NonterminalNodeFunction[SantaFeFunction](2) {
  def toIdentifier = "PROG2"
  def apply(children: Seq[ProgramNode[SantaFeFunction]]) = new ProgN(children.map(_.evaluate))
}

object Prog3NodeFunction extends NonterminalNodeFunction[SantaFeFunction](3) {
  def toIdentifier = "PROG3"
  def apply(children: Seq[ProgramNode[SantaFeFunction]]) = new ProgN(children.map(_.evaluate))
}

class IfFoodAhead(childFunctions: Seq[SantaFeFunction]) extends SantaFeFunction {
  def apply(world: SantaFeWorld) = if(world.foodAhead) childFunctions.head(world) else childFunctions.last(world)
}

object IfFoodAheadNodeFunction extends NonterminalNodeFunction[SantaFeFunction](2) {
  def toIdentifier = "IF-FOOD-AHEAD"
  def apply(children: Seq[ProgramNode[SantaFeFunction]]) = new IfFoodAhead(children.map(_.evaluate))
}
