package advent_of_code_2020.day4

import advent_of_code_2020.Solution
import advent_of_code_2020.day4.Passport.{isValid, isValidExtended}

object PassportProcessing extends Solution[Int] {

  override def day: Int = 4

  override def part1(): Int = readPassports.filter(isValid(_)).size

  def readPassports: List[Passport] = {
    normalizeMultilineInput().map(line => {
      Passport(
        readPassportItem("byr", line),
        readPassportItem("iyr", line),
        readPassportItem("eyr", line),
        readPassportItem("hgt", line),
        readPassportItem("hcl", line),
        readPassportItem("ecl", line),
        readPassportItem("pid", line),
        readPassportItem("cid", line)
      )
    })
  }

  def readPassportItem(item: String, line: String): Option[String] = {
    s"$item:(.*?)\\s".r.findFirstIn(line.concat(" ")) match {
      case Some(i) => {
        Some(i.split(":")(1).trim)
      }
      case None => None
    }
  }

  override def part2(): Int = readPassports.filter(isValidExtended(_)).size
}
