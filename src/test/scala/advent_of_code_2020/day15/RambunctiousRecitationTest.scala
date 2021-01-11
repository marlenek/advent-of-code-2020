package advent_of_code_2020.day15

import advent_of_code_2020.day15.RambunctiousRecitation.{number2020th, number30000000th}
import org.scalatest.{FlatSpec, Matchers}

class RambunctiousRecitationTest extends FlatSpec with Matchers {

  "rambunctious recitation" should "correctly calculate 2020th number" in {
    number2020th(Array(0, 3, 6)) should be(436)
    number2020th(Array(1, 3, 2)) should be(1)
    number2020th(Array(2, 1, 3)) should be(10)
    number2020th(Array(1, 2, 3)) should be(27)
    number2020th(Array(2, 3, 1)) should be(78)
    number2020th(Array(3, 2, 1)) should be(438)
    number2020th(Array(3, 1, 2)) should be(1836)
  }

  "rambunctious recitation" should "correctly calculate 30000000th number" in {
    number30000000th(Array(0, 3, 6)) should be(175594)
    number30000000th(Array(1, 3, 2)) should be(2578)
    number30000000th(Array(2, 1, 3)) should be(3544142)
    number30000000th(Array(1, 2, 3)) should be(261214)
    number30000000th(Array(2, 3, 1)) should be(6895259)
    number30000000th(Array(3, 2, 1)) should be(18)
    number30000000th(Array(3, 1, 2)) should be(362)
  }
}
