import org.specs2.mutable._
import org.specs2.specification.Scope

import com.darnowsky.scalagp.ProgramNode._
import com.darnowsky.scalagp.ProgramGenerationStrategy._

class FullGenerationStrategySpec extends Specification {
  "A FullGenerationStrategy" should {
    "generate all functional nodes in a branch node" in branch_node_context {
      pending
    }
    "initialize all children of a branch node it generates with another FullGenerationStrategy of decremented depth" in branch_node_context {
      pending
    }

    "generate all terminal nodes in a leaf node" in leaf_node_context {
      pending
    }
  }
}

object branch_node_context extends Before {
  def before = { val depth = 5 }
}

object leaf_node_context extends Before {
  def before = { val depth = 0 }
}
