package advent_of_code_2020.day5

import advent_of_code_2020.day5.BoardingCard.calculateRowAndColumn
import org.scalatest.{FlatSpec, Matchers}

class BoardingCardTest extends FlatSpec with Matchers {

  "boarding card" should "have correct seat" in {

    calculateRowAndColumn(BoardingCard("BFFFBBFRRR")) should be(70, 7)
    calculateRowAndColumn(BoardingCard("FFFBBBFRRR")) should be(14, 7)
    calculateRowAndColumn(BoardingCard("BBFFBBFRLL")) should be(102, 4)
    calculateRowAndColumn(BoardingCard("FFFFFFFLLL")) should be(0, 0)

  }
}
