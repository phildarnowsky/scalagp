import org.specs2.mutable._

import com.darnowsky.scalagp.ProgramNode._
import com.darnowsky.scalagp.ProgramGenerationStrategy._
import com.darnowsky.scalagp.NodeFunction._
import org.specs2.mock._
import scala.collection.immutable.Queue

class ProgramNodeSpec extends Specification with Mockito with SpecHelpers {
  def allNodeFunctions[T](programs: List[ProgramNode[T]]): List[NodeFunction[T]] = programs.flatMap(_.allDescendants).map(_.evaluationFunction)

  "A ProgramNode" should {      
    // TODO: make an easier way to set up these fixed programs

    val program1 =  new ProgramNode[Int](
      AddEvaluationFunction,
      Array(
        new ProgramNode[Int](
          ConstantEvaluationFunction42,
          Array():Array[ProgramNode[Int]],
          Queue(0)
        ),

        new ProgramNode[Int](
          NegateEvaluationFunction,
          Array(
            new ProgramNode[Int](
              ConstantEvaluationFunction666,
              Array():Array[ProgramNode[Int]],
              Queue(1, 0)
            )
          ),
          Queue(1)
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
                  Array():Array[ProgramNode[Int]],
                  Queue(0, 0, 0)
                )

              ),
              Queue(0, 0)
            ),

            new ProgramNode[Int](
              AddEvaluationFunction,
              Array(
                new ProgramNode[Int](
                  ConstantEvaluationFunction3456,
                  Array():Array[ProgramNode[Int]],
                  Queue(0, 1, 0)
                ),

                new ProgramNode[Int](
                  ConstantEvaluationFunction78910,
                  Array():Array[ProgramNode[Int]],
                  Queue(0, 1, 1)
                )
              ),
              Queue(0, 1)
            )
          ),
          Queue(0)
        )
      )
    )

    // program1 is (+ 42 (- 666))
    // program2 is (- (* (succ 1337) (+ 3456 78910)))

    "handle the case in which it crosses over at the root" in {
      val spiedProgram1 = spy(program1)
      val spiedProgram2 = spy(program2)

      spiedProgram1.chooseRandomDescendant().returns(spiedProgram1) // choose the root
      spiedProgram2.chooseRandomDescendant().returns(spiedProgram2.children(0).children(1)) // choose (+ 3456 78910)
      val(child1, child2) = spiedProgram1 crossoverWith spiedProgram2

      child1.evaluate() mustEqual (3456 + 78910)
      child2.evaluate() mustEqual -(1338 * (42 - 666))

      child1.toSExpression mustEqual "(+ 3456 78910)"
      child2.toSExpression mustEqual "(- (* (succ 1337) (+ 42 (- 666))))"

      child1.pathFromRoot.isEmpty must beTrue
      child1.children(0).pathFromRoot mustEqual Queue(0)
      child1.children(1).pathFromRoot mustEqual Queue(1)

      child2.pathFromRoot.isEmpty must beTrue // -
      child2.children(0).pathFromRoot mustEqual Queue(0) // *
      child2.children(0).children(0).pathFromRoot mustEqual Queue(0, 0) // succ
      child2.children(0).children(0).children(0).pathFromRoot mustEqual Queue(0, 0, 0) // 1337
      child2.children(0).children(1).pathFromRoot mustEqual Queue(0, 1) // +
      child2.children(0).children(1).children(0).pathFromRoot mustEqual Queue(0, 1, 0) // 42
      child2.children(0).children(1).children(1).pathFromRoot mustEqual Queue(0, 1, 1) // -
      child2.children(0).children(1).children(1).children(0).pathFromRoot mustEqual Queue (0, 1, 1, 0) // 666
    }

    "be able to do crossover reproduction with another ProgramNode" in {  
      val spiedProgram1 = spy(program1)
      val spiedProgram2 = spy(program2)

      spiedProgram1.chooseRandomDescendant().returns(spiedProgram1.children(0)) // choose the 42 node
      spiedProgram2.chooseRandomDescendant().returns(spiedProgram2.children(0).children(1)) // choose (+ 3456 78910)
      val(child1, child2) = spiedProgram1 crossoverWith spiedProgram2

      child1.evaluate() mustEqual (3456 + 78910 - 666)
      child2.evaluate() mustEqual -(1338 * 42)

      child1.toSExpression mustEqual "(+ (+ 3456 78910) (- 666))"
      child2.toSExpression mustEqual "(- (* (succ 1337) 42))"

      child1.pathFromRoot.isEmpty must beTrue // outer +
      child1.children(0).pathFromRoot mustEqual Queue(0) // inner +
      child1.children(0).children(0).pathFromRoot mustEqual Queue(0, 0) // 3456
      child1.children(0).children(1).pathFromRoot mustEqual Queue(0, 1) // 78910
      child1.children(1).pathFromRoot mustEqual Queue(1) // -
      child1.children(1).children(0).pathFromRoot mustEqual Queue(1, 0) // 666

      child2.pathFromRoot.isEmpty must beTrue // -
      child2.children(0).pathFromRoot mustEqual Queue(0) // *
      child2.children(0).children(0).pathFromRoot mustEqual Queue(0, 0) // succ
      child2.children(0).children(0).children(0).pathFromRoot mustEqual Queue(0, 0, 0) // 1337
      child2.children(0).children(1).pathFromRoot mustEqual Queue(0, 1) // 42
    }  

    "be able to evaluate itself" in {
      val constantNode = new ProgramNode[Int](ConstantEvaluationFunction42, Array():Array[ProgramNode[Int]])
      constantNode.evaluate() mustEqual 42
    }

    "be able to evaluate itself as a function of its children" in {
      val leftChild = new ProgramNode[Int](ConstantEvaluationFunction42, Array():Array[ProgramNode[Int]], Queue(0))
      val rightChild = new ProgramNode[Int](ConstantEvaluationFunction666, Array():Array[ProgramNode[Int]], Queue(1))
      val topNode = new ProgramNode[Int](AddEvaluationFunction, Array(leftChild, rightChild))
      topNode.evaluate() mustEqual (42 + 666)
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
      new FakeProgramGenerationStrategy(3),
      Queue()
    )


    "use the children assigned to it by the ProgramGenerationStrategy it's initialized with" in {
      topNode.children.length mustEqual 2
      topNode.children.forall(_.evaluationFunction == AddEvaluationFunction) must beTrue

      val grandchildren = topNode.children.flatMap(_.children)
      grandchildren.length mustEqual 4
      grandchildren.forall(_.evaluationFunction == ConstantEvaluationFunction42) must beTrue
    }

    "know the path from the root to here" in {
      topNode.pathFromRoot mustEqual Queue()
      topNode.children(0).pathFromRoot mustEqual Queue(0)
      topNode.children(1).pathFromRoot mustEqual Queue(1)

      topNode.children(0).children(0).pathFromRoot mustEqual Queue(0, 0)
      topNode.children(0).children(1).pathFromRoot mustEqual Queue(0, 1)
      topNode.children(1).children(0).pathFromRoot mustEqual Queue(1, 0)
      topNode.children(1).children(1).pathFromRoot mustEqual Queue(1, 1)
    }

    "return a flattened list of its descendents" in {
      val descendantList = topNode.allDescendants

      descendantList.length mustEqual 7
      descendantList.contains(topNode) must beTrue
      descendantList.contains(topNode.children(0)) must beTrue
      descendantList.contains(topNode.children(1)) must beTrue
      descendantList.contains(topNode.children(0).children(0)) must beTrue
      descendantList.contains(topNode.children(0).children(1)) must beTrue
      descendantList.contains(topNode.children(1).children(0)) must beTrue
      descendantList.contains(topNode.children(1).children(1)) must beTrue
    }
  }
}
