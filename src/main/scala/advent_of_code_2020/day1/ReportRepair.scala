package advent_of_code_2020.day1

import advent_of_code_2020.Solution

object ReportRepair extends Solution[Int] {

  val bestYearEver: Int = 2020
  val input: List[Int]  = inputAsStringLines.map(_.toInt).toList

  override def part1(): Int = findTwoEntries(input, bestYearEver).get

  def findTwoEntries(input: List[Int], sum: Int): Option[Int] = {
    input.foreach(n => {
      if (input.contains(sum - n))
        return Some(n * (sum - n))
    })
    None
  }

  override def part2(): Int = findThreeEntries(input).get

  def findThreeEntries(input: List[Int]): Option[Int] = {
    var i    = 0
    val size = input.size
    input.foreach(n => {
      {
        val sumOfTwo = bestYearEver - n
        val d        = findTwoEntries(input.slice(i, size), sumOfTwo)
        if (d.isDefined) return Some(d.get * n)
        i = i + 1
      }
    })
    None
  }

  override def day: Int = 1
}
