package advent_of_code_2020.day6

import advent_of_code_2020.Solution

object CustomCustoms extends Solution[Int] {
  override def day: Int = 6

  override def part1(): Int = {
    normalizeMultilineInput().map(line => line.replace(" ", "").toCharArray.distinct.length).sum
  }

  override def part2(): Int = {
    normalizeMultilineInput()
      .map(line => {
        val numberOfAnswers = line.toCharArray.count(char => char == ' ') + 1
        line.toCharArray.filter(char => char != ' ').groupBy(identity).view.mapValues(_.size).count(v => v._2 == numberOfAnswers)
      })
      .sum

  }
}
