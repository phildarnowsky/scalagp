package com.darnowsky.scalagp.FullGenerationStrategy

import com.darnowsky.scalagp.ProgramGenerationStrategy._

class FullGenerationStrategy[T] extends ProgramGenerationStrategy[T] {
  def generateChildren(depth: Int) = {
    List()
  }
}
