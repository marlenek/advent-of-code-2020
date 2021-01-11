package advent_of_code_2020.day5

import advent_of_code_2020.Solution
import advent_of_code_2020.day5.BoardingCard.calculateSeatNumber

object BinaryBoarding extends Solution[Int] {
  override def day: Int = 5

  override def part2(): Int = {
    val minSeat = inputAsStringLines.map(BoardingCard(_)).map(calculateSeatNumber(_)).min + 1
    val maxSeat = part1() - 1
    (minSeat to maxSeat).toList.diff(inputAsStringLines.map(BoardingCard(_)).map(calculateSeatNumber(_)).toList).head
  }

  override def part1(): Int = inputAsStringLines.map(BoardingCard(_)).map(calculateSeatNumber(_)).max
}
