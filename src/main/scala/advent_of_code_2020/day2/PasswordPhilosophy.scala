package advent_of_code_2020.day2

import advent_of_code_2020.Solution

object PasswordPhilosophy extends Solution[Int] {

  val databaseEntryList: List[DatabaseEntry] = {
    inputAsStringLines.map { line =>
      DatabaseEntry(
        line.split('-')(0).toInt,
        "\\d*(?= )".r.findFirstIn(line).get.toInt,
        line.split(" ")(1).split(":")(0).toCharArray.head,
        line.split(" ")(2).trim
      )
    }.toList
  }

  override def day: Int = 2

  override def part1(): Int = {
    databaseEntryList.count(entry => {
      val letterCount = entry.password.toCharArray.count(char => char == entry.letter)
      letterCount >= entry.min && letterCount <= entry.max
    })
  }

  override def part2(): Int = {
    databaseEntryList.count(entry => {
      val firstPos  = checkCharAtPositionExists(entry.letter, entry.min - 1, entry.password)
      val secondPos = checkCharAtPositionExists(entry.letter, entry.max - 1, entry.password)
      (firstPos || secondPos) && !(firstPos && secondPos)
    })
  }

  def checkCharAtPositionExists(char: Char, position: Int, word: String): Boolean = {
    if (word.length >= position) {
      word.charAt(position).equals(char)
    } else false
  }

}
