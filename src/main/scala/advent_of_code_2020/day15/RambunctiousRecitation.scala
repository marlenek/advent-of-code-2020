package advent_of_code_2020.day15

import advent_of_code_2020.Solution

object RambunctiousRecitation extends Solution[Int] {
  override def day: Int = 15

  override def part1(): Int = {
    number2020th(Array(1, 12, 0, 20, 8, 16))
  }

  // too slow for part2
  def number2020th(initialNumbers: Array[Int]): Int = {
    val steps = 2020
    var game  = initialNumbers.clone()
    for (i <- initialNumbers.length to steps) {
      var newNumber  = 0
      val prevNumber = game(i - 1)
      if (game.count(n => n == prevNumber) >= 2) {
        newNumber = i - game.lastIndexOf(prevNumber, i - 2) - 1
      }
      game = game :+ newNumber
    }
    game(steps - 1)
  }

  override def part2(): Int = number30000000th(Array(1, 12, 0, 20, 8, 16))

  // could be used for both parts
  def number30000000th(initialNumbers: Array[Int]): Int = {
    val steps = 30000000
    var map   = scala.collection.mutable.Map[Int, (Option[Int], Option[Int])]()
    initialNumbers.foreach(nr => map = map + (nr -> (None, Some(initialNumbers.indexOf(nr)))))
    var prevNumber = initialNumbers.last
    for (i <- initialNumbers.length until steps) {
      var newNumber = 0
      if (map.contains(prevNumber) && map(prevNumber)._1.isDefined) {
        val min = map(prevNumber)._1.get
        val max = map(prevNumber)._2.get
        newNumber = max - min
      }
      if (map.contains(newNumber)) {
        val max = map(newNumber)._2
        map.update(newNumber, (max, Some(i)))
      } else
        map.addOne(newNumber, (None, Some(i)))
      prevNumber = newNumber
    }
    prevNumber
  }
}
