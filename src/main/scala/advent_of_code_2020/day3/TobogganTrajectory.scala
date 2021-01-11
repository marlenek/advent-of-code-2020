package advent_of_code_2020.day3

import advent_of_code_2020.Solution

object TobogganTrajectory extends Solution[Long] {

  val input: Array[String] = {
    inputAsStringLines.toArray
  }

  override def day: Int = 3

  override def part1(): Long = checkTreesInPath(input, 3, 1)

  def checkTreesInPath(input: Array[String], right: Int, down: Int): Long = {
    var numberOfTrees = 0
    var position      = 0
    val lineLength    = input.head.length

    for (i <- 0 until input.length by down) {
      if (input(i).charAt(position) == '#')
        numberOfTrees = numberOfTrees + 1
      position = (position + right) % lineLength
    }
    numberOfTrees
  }

  override def part2(): Long = {
    val paths = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
    paths.map(path => checkTreesInPath(input, path._1, path._2)).product
  }

}
