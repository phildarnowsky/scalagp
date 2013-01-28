package com.darnowsky.scalagp_examples.nodefunction.Polynomial

import com.darnowsky.scalagp.NodeFunction._
import com.darnowsky.scalagp.ProgramNode.ProgramNode

class Monomial(val coefficient:Double, val power:Int) {
  override def toString = {
    if(coefficient == 0.0)
      "0"
    else
      power match {
        case 0 => coefficient.toString
        case 1 => coefficient.toString ++ "X"
        case _ => coefficient.toString ++ "X^" ++ power.toString
      }
  }

  def * (that: Monomial): Monomial = {
    new Monomial(this.coefficient * that.coefficient, this.power + that.power)
  }

  def + (that: Monomial): Polynomial = new Polynomial(List(this, that))

  def apply(x: Double): Double = coefficient * scala.math.pow(x, power)
}

object Monomial {
  def X = new Monomial(1, 1)
}

class Polynomial(val terms:Seq[Monomial]) {
  override def toString = terms.map(_.toString) mkString " + "

  def * (that: Polynomial): Polynomial = {
    val newTerms = this.terms.flatMap((leftTerm) => that.terms.map((rightTerm) => leftTerm * rightTerm))
    new Polynomial(newTerms)
  }

  def + (that: Polynomial): Polynomial = {
    new Polynomial(this.terms ++ that.terms)
  }

  def apply(x: Double): Double = terms.map(_(x)).sum
}

abstract class BinaryOperator extends NonterminalNodeFunction[Polynomial](2) {
}

object Add extends BinaryOperator {
  def apply(children: Seq[ProgramNode[Polynomial]]): Polynomial = {
    children(0).evaluate + children(1).evaluate
  }

  def toIdentifier = "+"
}

object Multiply extends BinaryOperator {
  def apply(children: Seq[ProgramNode[Polynomial]]): Polynomial = {
    children(0).evaluate * children(1).evaluate
  }

  def toIdentifier = "*"
}

object IndependentVariable extends TerminalNodeFunction[Polynomial] {
  def apply(children: Seq[ProgramNode[Polynomial]]) = new Polynomial(List(Monomial.X))

  def toIdentifier = "X"
}

class ERCNodeFunction(lower: Double, upper: Double) extends TerminalNodeFunction[Polynomial] {
  require(upper > lower)
  val spread = upper - lower
  lazy val constantValue = (new scala.util.Random).nextDouble * spread + lower

  def apply(_children: Seq[ProgramNode[Polynomial]]) = new Polynomial(List(new Monomial(constantValue, 0)))

  def toIdentifier = constantValue.toString
}

class ERCNodeFunctionCreator(lower: Double, upper: Double) extends TerminalNodeFunctionCreator[Polynomial] {
  def getNodeFunction(): ERCNodeFunction = new ERCNodeFunction(lower, upper)
}
