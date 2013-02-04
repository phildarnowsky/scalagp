package com.darnowsky.scalagp_examples.nodefunction.booleanfunction.ElevenMultiplexer

import com.darnowsky.scalagp.NodeFunction._
import com.darnowsky.scalagp.ProgramNode.ProgramNode

abstract class BooleanFunction extends (IndexedSeq[Boolean] => Boolean) {
}

class AndFunction(children: IndexedSeq[BooleanFunction]) extends BooleanFunction {
  def apply(arguments: IndexedSeq[Boolean]) = children(0)(arguments) && children(1)(arguments)
}

class OrFunction(children: IndexedSeq[BooleanFunction]) extends BooleanFunction {
  def apply(arguments: IndexedSeq[Boolean]) = children(0)(arguments) || children(1)(arguments)
}

class NotFunction(children: IndexedSeq[BooleanFunction]) extends BooleanFunction {
  def apply(arguments: IndexedSeq[Boolean]) = !(children(0)(arguments))
}

class IfFunction(children: IndexedSeq[BooleanFunction]) extends BooleanFunction {
  def apply(arguments: IndexedSeq[Boolean]) = if(children(0)(arguments)) children(1)(arguments) else children(2)(arguments)
}

class LineFunction(lineIndex: Int) extends BooleanFunction {
  def apply(lines: IndexedSeq[Boolean]) = lines(lineIndex)
}

object AndNodeFunction extends NonterminalNodeFunction[BooleanFunction](2) {
  def apply(children: Seq[ProgramNode[BooleanFunction]]) = new AndFunction(children.map(_.evaluate).toIndexedSeq)
  def toIdentifier = "AND"
}

object OrNodeFunction extends NonterminalNodeFunction[BooleanFunction](2) {
  def apply(children: Seq[ProgramNode[BooleanFunction]]) = new OrFunction(children.map(_.evaluate).toIndexedSeq)
  def toIdentifier = "OR"
}

object NotNodeFunction extends NonterminalNodeFunction[BooleanFunction](1) {
  def apply(children: Seq[ProgramNode[BooleanFunction]]) = new NotFunction(children.map(_.evaluate).toIndexedSeq)
  def toIdentifier = "NOT"
}

object IfNodeFunction extends NonterminalNodeFunction[BooleanFunction](3) {
  def apply(children: Seq[ProgramNode[BooleanFunction]]) = new IfFunction(children.map(_.evaluate).toIndexedSeq)
  def toIdentifier = "IF"
}


/* To make life simpler, we adopt the convention that lines 0-2 are the
   address lines, with 2 as the MSB, and 3-10 are the data lines, with 10 as
   the MSB. */

class AddressLineNodeFunction(lineIndex: Int) extends TerminalNodeFunction[BooleanFunction] {
  def toIdentifier = "A" ++ lineIndex.toString
  def apply(_children: Seq[ProgramNode[BooleanFunction]]) = new LineFunction(lineIndex)
}

class DataLineNodeFunction(lineIndex: Int) extends TerminalNodeFunction[BooleanFunction] {
  def toIdentifier = "D" ++ lineIndex.toString
  def apply(_children: Seq[ProgramNode[BooleanFunction]]) = new LineFunction(lineIndex + 8)
}

object Address0NodeFunction extends AddressLineNodeFunction(0) {
}

object Address1NodeFunction extends AddressLineNodeFunction(1) {
}

object Address2NodeFunction extends AddressLineNodeFunction(2) {
}

object Data0NodeFunction extends DataLineNodeFunction(0) {
}

object Data1NodeFunction extends DataLineNodeFunction(1) {
}

object Data2NodeFunction extends DataLineNodeFunction(2) {
}

object Data3NodeFunction extends DataLineNodeFunction(3) {
}

object Data4NodeFunction extends DataLineNodeFunction(4) {
}

object Data5NodeFunction extends DataLineNodeFunction(5) {
}

object Data6NodeFunction extends DataLineNodeFunction(6) {
}

object Data7NodeFunction extends DataLineNodeFunction(7) {
}
