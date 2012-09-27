package com.darnowsky.scalagp.ProgramGenerationStrategy

import com.darnowsky.scalagp.ProgramNode.ProgramNode

trait ProgramGenerationStrategy[T] {
  def generateChildren(depth: Int): Seq[ProgramNode[T]]
}
