package advent_of_code_2020.day9

import advent_of_code_2020.Solution

object EncodingError extends Solution[Long] {

  val inputNumbers = inputAsStringLines.map(line => line.toLong).toSeq

  override def day: Int = 9

  override def part1(): Long = {
    var index = 25
    while (checkIfSumOfPrev20(index)) {
      index = index + 1
    }
    inputNumbers(index)
  }

  def checkIfSumOfPrev20(index: Int): Boolean = {
    val sum          = inputNumbers(index)
    val numbersToSum = inputNumbers.slice(index - 25, index)
    numbersToSum.foreach(nr => {
      if (numbersToSum.contains(sum - nr) && nr * 2 != sum)
        return true
    })
    false
  }

  override def part2(): Long = {
    val sumToFind     = 133015568
    var startingIndex = 0
    var endingIndex   = 0
    while (partialSum(startingIndex, endingIndex) != sumToFind) {
      partialSum(startingIndex, endingIndex) match {
        case x if x < sumToFind => endingIndex = endingIndex + 1
        case x if x > sumToFind => startingIndex = startingIndex + 1
      }
    }
    inputNumbers.slice(startingIndex, endingIndex).min + inputNumbers
      .slice(startingIndex, endingIndex)
      .max
  }

  def partialSum(startingIndex: Int, endingIndex: Int): Long = {
    inputNumbers.slice(startingIndex, endingIndex).sum
  }
}
