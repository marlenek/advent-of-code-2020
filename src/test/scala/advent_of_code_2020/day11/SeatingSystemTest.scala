package advent_of_code_2020.day11

import advent_of_code_2020.day11.SeatingSystem.{
  directionSeats,
  occupiedSeatsAtTheEnd,
  readSeatingArea
}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class SeatingSystemTest extends FlatSpec with Matchers {

  "seating system" should "have 37 occupied seats at the end for part 1" in {
    val testInput = Source.fromURL(getClass.getResource("/input11.txt")).getLines()
    occupiedSeatsAtTheEnd(readSeatingArea(testInput), 1) should be(37)
  }

  "seating system" should "have 26 occupied seats at the end for part 2" in {
    val testInput   = Source.fromURL(getClass.getResource("/input11.txt")).getLines()
    val seatingArea = readSeatingArea(testInput)
    directionSeats(seatingArea, 0, 0) should contain theSameElementsAs List('L', 'L', 'L')
    occupiedSeatsAtTheEnd(seatingArea, 2) should be(26)
  }

}
