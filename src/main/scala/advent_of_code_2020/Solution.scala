package advent_of_code_2020

import scala.io.Source

trait Solution[T] {

  def day: Int

  def part1(): T

  def part2(): T

  def main(args: Array[String]): Unit = {

    printResults()
  }

  def printResults(): Unit = {
    println("Result for part 1: " + part1())
    println("Result for part 2: " + part2())
  }

  def normalizeMultilineInput(): List[String] = {
    inputAsStringLines.toList
      .foldLeft(List[String]()) {
        case (Nil, str) => str :: Nil
        case (head :: tail, str) =>
          if (!head.isEmpty && !str.isEmpty) (s"$head $str") :: tail
          else str :: head :: tail
      }
      .reverse
      .filter(line => !line.isEmpty)
  }

  def inputAsStringLines: Iterator[String] =
    Source.fromResource(s"advent_of_code_2020/input$day.txt").getLines()
}
