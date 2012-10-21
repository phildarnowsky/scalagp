import org.specs2.mutable._
import org.specs2.mock._

import com.darnowsky.scalagp.ProgramGenerationStrategy._
import com.darnowsky.scalagp.NodeFunction.{TerminalNodeFunction, NonterminalNodeFunction}
import com.darnowsky.scalagp.FullGenerationStrategy.FullGenerationStrategy
import com.darnowsky.scalagp.GrowGenerationStrategy.GrowGenerationStrategy
import com.darnowsky.scalagp.ProgramNode.ProgramNode
import com.darnowsky.scalagp.Population._

class PopulationSpec extends Specification with Mockito {
  "The Population companion object" should {
    "be able to generate a Population via ramped even splits" in {
      object Nonterminal1 extends NonterminalNodeFunction[Int](2) {
        def apply(nodes: Seq[ProgramNode[Int]]) = nodes.head.evaluate
        def toIdentifier = "first"
      }

      object Nonterminal2 extends NonterminalNodeFunction[Int](2) {
        def apply(nodes: Seq[ProgramNode[Int]]) = nodes.last.evaluate
        def toIdentifier = "last"
      }

      object Terminal1 extends TerminalNodeFunction[Int] {
        def apply(nodes: Seq[ProgramNode[Int]]) = 123
        def toIdentifier = "123"
      }

      object Terminal2 extends TerminalNodeFunction[Int] {
        def apply(nodes: Seq[ProgramNode[Int]]) = 456
        def toIdentifier = "456"
      }

      val fullStrategy = spy(new FullGenerationStrategy[Int](List(Nonterminal1, Nonterminal2), List(Terminal1, Terminal2)))
      val growStrategy = spy(new GrowGenerationStrategy[Int](List(Nonterminal1, Nonterminal2), List(Terminal1, Terminal2)))

      val population = Population.generate(3, 5, List(fullStrategy, growStrategy))

      (1 to 5).foreach(expectedDepth => there was 3.times(fullStrategy).generateProgram(expectedDepth))
      (1 to 5).foreach(expectedDepth => there was 3.times(growStrategy).generateProgram(expectedDepth))
    }
  }
}
