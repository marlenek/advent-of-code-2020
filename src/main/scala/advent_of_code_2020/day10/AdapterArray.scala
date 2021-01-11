package advent_of_code_2020.day10

import advent_of_code_2020.Solution

object AdapterArray extends Solution[Long] {

  val inputNumbers: Seq[Int] = inputAsStringLines.map(line => line.toInt).toSeq

  override def day: Int = 10

  override def part1(): Long = {
    calcAdapterMultiDiff(inputNumbers)
  }

  def calcAdapterMultiDiff(input: Seq[Int]): Long = {
    val adapters = createAdaptersList(input)
    var ones     = 0
    var thirds   = 0
    for (i <- 0 to (adapters.length - 2)) {
      adapters(i + 1) - adapters(i) match {
        case 1 =>
          ones = ones + 1
        case 3 =>
          thirds = thirds + 1
      }
    }
    ones * thirds
  }

  def createAdaptersList(input: Seq[Int]): Seq[Int] = {
    input.sorted.prepended(0).appended(input.max + 3)
  }

  override def part2(): Long = {
    calcVariations(inputNumbers)
  }

  def calcVariations(input: Seq[Int]): Long = {
    val adapters         = createAdaptersList(input)
    var ones             = 0
    var list: List[Long] = List.empty
    for (i <- 0 to (adapters.length - 2)) {
      adapters(i + 1) - adapters(i) match {
        case 1 =>
          ones = ones + 1
        case 3 =>
          list = list :+ ones
          ones = 0
      }
    }
    list.map {
      case 0 => 1
      case 1 => 1
      case 2 => 2
      case 3 => 4
      case 4 => 7
    }.map(_.toLong).product
  }

}
