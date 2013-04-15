package com.darnowsky.scalagp_examples.nodefunction.santa_fe.SantaFe

import com.darnowsky.scalagp.NodeFunction._
import com.darnowsky.scalagp.ProgramNode.ProgramNode
import scala.annotation.tailrec

object SantaFeWorld {
  def fromString(stringMap: String, position: (Int, Int) = (0, 0), heading: (Int, Int) = (0, 1)): SantaFeWorld = {
    val stringRows = stringMap.split("\n")
    val rows = stringRows.map((row) => row.map((char) => char == 'X')).toIndexedSeq
    val worldmap = rows.toIndexedSeq

    new SantaFeWorld(position, heading, worldmap)
  }

  def defaultTrail(): SantaFeWorld = fromString(
"""|.XXX............................
   |...X............................
   |...X.....................XXX....
   |...X....................X....X..
   |...X....................X....X..
   |...XXXX.XXXXX........XX.........
   |............X................X..
   |............X.......X...........
   |............X.......X...........
   |............X.......X........X..
   |....................X...........
   |............X...................
   |............X................X..
   |............X.......X...........
   |............X.......X.....XXX...
   |.................X.....X........
   |................................
   |............X...................
   |............X...X.......X.......
   |............X...X..........X....
   |............X...X...............
   |............X...X...............
   |............X.............X.....
   |............X..........X........
   |...XX..XXXXX....X...............
   |.X..............X...............
   |.X..............X...............
   |.X......XXXXXXX.................
   |.X.....X........................
   |.......X........................
   |..XXXX..........................
   |................................""".stripMargin
  )
}

case class SantaFeWorld(position: (Int, Int) = (0, 0), heading: (Int, Int) = (0, 1), worldmap: IndexedSeq[IndexedSeq[Boolean]], stepsRemaining: Int = 400) {
  /* Initial position is the upper-left-hand corner, and we use (down, right) coordinates
     Initial heading is east
     Boolean entries in worldmap indicate if there is a piece of food at the location */

  val height = worldmap.length
  val width = worldmap.head.length

  def moveForward(): SantaFeWorld = {
    if(this.outOfTime) 
      this
    else {
      val newMap = worldmap.updated(ahead._1, worldmap(ahead._1).updated(ahead._2, false)) // eat any food at the new location
      this.copy(position = ahead, worldmap = newMap).tick
    }
  }

  def turnRight(): SantaFeWorld = {
    if(this.outOfTime)
      this
    else {
      val newHeading = heading match {
        case (0, 1)  => (1, 0)
        case (1, 0)  => (0, -1)
        case (0, -1) => (-1, 0)
        case (-1, 0) => (0, 1)
      }
      this.copy(heading = newHeading).tick
    }
  }

  def turnLeft(): SantaFeWorld = {
    if(this.outOfTime)
      this
    else {
      val newHeading = heading match {
        case (0, 1)  => (-1, 0)
        case (-1, 0) => (0, -1)
        case (0, -1) => (1, 0)
        case (1, 0)  => (0, 1)
      }
      this.copy(heading = newHeading).tick
    }
  }

  def foodAhead(): Boolean = {
    worldmap(ahead._1)(ahead._2)
  }

  val foodRemaining = worldmap.flatten.count(_ == true)

  val isEmpty = foodRemaining == 0

  val outOfTime = stepsRemaining == 0

  protected

  def ahead(): (Int, Int) = {
    val newRow = (position._1 + heading._1 + height) % height
    val newColumn = (position._2 + heading._2 + width) % width
    (newRow, newColumn)
  }

  def tick() = this.copy(stepsRemaining = stepsRemaining - 1)
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
