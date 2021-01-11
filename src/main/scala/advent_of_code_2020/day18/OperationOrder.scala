package advent_of_code_2020.day18

import advent_of_code_2020.Solution

import scala.collection.immutable

object OperationOrder extends Solution[Int] {

  override def day: Int = 18

  def solveMath(input: List[String]): Int = {
    input.foreach(line => {
      val splittedLine = line.replace(" ", "").toCharArray
      val queue= immutable.Queue(splittedLine)
      queue

    })


    1
  }


  override def part1(): Int = 0

  override def part2(): Int = 0
}
