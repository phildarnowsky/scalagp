import org.specs2.mutable._

import com.darnowsky.scalagp.ProgramNode._
import com.darnowsky.scalagp.ProgramGenerationStrategy._
import com.darnowsky.scalagp.NodeFunction._
import org.specs2.mock._
import scala.collection.immutable.Queue

class ProgramNodeSpec extends Specification with Mockito with SpecHelpers {
  def allNodeFunctions[T](programs: List[ProgramNode[T]]): List[NodeFunction[T]] = programs.flatMap(_.allDescendants).map(_._1.evaluationFunction)

  "A ProgramNode" should {      
    // TODO: make an easier way to set up these fixed programs

    val program1 =  new ProgramNode[Int](
      AddEvaluationFunction,
      Array(
        new ProgramNode[Int](
          ConstantEvaluationFunction42,
          Array():Array[ProgramNode[Int]]
        ),

        new ProgramNode[Int](
          NegateEvaluationFunction,
          Array(
            new ProgramNode[Int](
              ConstantEvaluationFunction666,
              Array():Array[ProgramNode[Int]]
            )
          )
        )
      )
    )

    val program2 = new ProgramNode[Int](
      NegateEvaluationFunction,
      Array(
        new ProgramNode[Int](
          MultiplyEvaluationFunction,
          Array(

            new ProgramNode[Int](
              SuccessorEvaluationFunction,
              Array(

                new ProgramNode[Int](
                  ConstantEvaluationFunction1337,
                  Array():Array[ProgramNode[Int]]
                )

              )
            ),

            new ProgramNode[Int](
              AddEvaluationFunction,
              Array(
                new ProgramNode[Int](
                  ConstantEvaluationFunction3456,
                  Array():Array[ProgramNode[Int]]
                ),

                new ProgramNode[Int](
                  ConstantEvaluationFunction78910,
                  Array():Array[ProgramNode[Int]]
                )
              )
            )
          )
        )
      )
    )

    // program1 is (+ 42 (- 666))
    // program2 is (- (* (succ 1337) (+ 3456 78910)))

    "handle the case in which it crosses over at the root" in {
      val spiedProgram1 = spy(program1)
      val spiedProgram2 = spy(program2)

      spiedProgram1.chooseRandomDescendant().returns((spiedProgram1, Queue())) // choose the root
      spiedProgram2.chooseRandomDescendant().returns((spiedProgram2.children(0).children(1), Queue(0, 1))) // choose (+ 3456 78910)
      val children = spiedProgram1 crossoverWith spiedProgram2
      val child1 = children(0)
      val child2 = children(1)

      child1.evaluate mustEqual (3456 + 78910)
      child2.evaluate mustEqual -(1338 * (42 - 666))

      child1.toSExpression mustEqual "(+ 3456 78910)"
      child2.toSExpression mustEqual "(- (* (succ 1337) (+ 42 (- 666))))"
    }

    "be able to do crossover reproduction with another ProgramNode" in {
      val spiedProgram1 = spy(program1)
      val spiedProgram2 = spy(program2)

      spiedProgram1.chooseRandomDescendant().returns((spiedProgram1.children(0), Queue(0))) // choose the 42 node
      spiedProgram2.chooseRandomDescendant().returns((spiedProgram2.children(0).children(1), Queue(0, 1))) // choose (+ 3456 78910)
      val children = spiedProgram1 crossoverWith spiedProgram2
      val child1 = children(0)
      val child2 = children(1)

      child1.evaluate mustEqual (3456 + 78910 - 666)
      child2.evaluate mustEqual -(1338 * 42)

      child1.toSExpression mustEqual "(+ (+ 3456 78910) (- 666))"
      child2.toSExpression mustEqual "(- (* (succ 1337) 42))"
    }  

    "enforce a limit on maximum crossover depth, which if breached the first parent gets returned" in {
      val spiedProgram1 = spy(program1)
      val spiedProgram2 = spy(program2)
    // program1 is (+ 42 (- 666))
    // program2 is (- (* (succ 1337) (+ 3456 78910)))

      spiedProgram1.chooseRandomDescendant().returns((spiedProgram1.children(1), Queue(1))) // choose the (- 666) node
      spiedProgram2.chooseRandomDescendant().returns((spiedProgram2, Queue())) // choose the root

      // crossover would yield a program with depth 5 as the first child
      spiedProgram1.crossoverWith(spiedProgram2, Some(4))(0) mustEqual spiedProgram1

      // crossover would yield a program with depth 2 as the second child
      spiedProgram1.crossoverWith(spiedProgram2, Some(1))(1) mustEqual spiedProgram1
    }

    "be able to evaluate itself" in {
      val constantNode = new ProgramNode[Int](ConstantEvaluationFunction42, Array():Array[ProgramNode[Int]])
      constantNode.evaluate mustEqual 42
    }

    "be able to evaluate itself as a function of its children" in {
      val leftChild = new ProgramNode[Int](ConstantEvaluationFunction42, Array():Array[ProgramNode[Int]])
      val rightChild = new ProgramNode[Int](ConstantEvaluationFunction666, Array():Array[ProgramNode[Int]])
      val topNode = new ProgramNode[Int](AddEvaluationFunction, Array(leftChild, rightChild))
      topNode.evaluate mustEqual (42 + 666)
    }
  }

  "A ProgramNode initialized by way of a ProgramGenerationStrategy" should {
    class FakeProgramGenerationStrategy(depth: Int) extends ProgramGenerationStrategy[Int](List(), List(), depth) {
      def chooseTerminalFunction(): NodeFunctionCreator[Int] = { ConstantEvaluationFunction42 }
      def chooseNonterminalFunction(): NodeFunctionCreator[Int] = { AddEvaluationFunction }
      def successor(): FakeProgramGenerationStrategy = new FakeProgramGenerationStrategy(depth - 1)
    }

    val topNode = new ProgramNode[Int](
      AddEvaluationFunction,
      new FakeProgramGenerationStrategy(3)
    )


    "use the children assigned to it by the ProgramGenerationStrategy it's initialized with" in {
      topNode.children.length mustEqual 2
      topNode.children.forall(_.evaluationFunction == AddEvaluationFunction) must beTrue

      val grandchildren = topNode.children.flatMap(_.children)
      grandchildren.length mustEqual 4
      grandchildren.forall(_.evaluationFunction == ConstantEvaluationFunction42) must beTrue
    }

    "return a flattened list of its descendents, each wrapped in a tuple that has the path to it attached" in {
      val descendantList = topNode.allDescendants

      descendantList.length mustEqual 7
      descendantList.contains((topNode, Queue())) must beTrue
      descendantList.contains((topNode.children(0), Queue(0))) must beTrue
      descendantList.contains((topNode.children(1), Queue(1))) must beTrue
      descendantList.contains((topNode.children(0).children(0), Queue(0, 0))) must beTrue
      descendantList.contains((topNode.children(0).children(1), Queue(0, 1))) must beTrue
      descendantList.contains((topNode.children(1).children(0), Queue(1, 0))) must beTrue
      descendantList.contains((topNode.children(1).children(1), Queue(1, 1))) must beTrue
    }

    "be able to calculate the depth of the subtree rooted at itself" in {
      topNode.depth mustEqual 3
      topNode.children(0).depth mustEqual 2
      topNode.children(1).depth mustEqual 2
      topNode.children(0).children(0).depth mustEqual 1
      topNode.children(0).children(1).depth mustEqual 1
      topNode.children(1).children(0).depth mustEqual 1
      topNode.children(1).children(1).depth mustEqual 1
    }
  }
}
