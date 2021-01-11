package advent_of_code_2020.day18

import org.scalatest.{FlatSpec, Matchers}

class OperationOrderTest extends FlatSpec with Matchers {

  "operation order" should "correctly solve weird math" in {
    OperationOrder.solveMath(List("2 * 3 + (4 * 5)"))
  }

}
