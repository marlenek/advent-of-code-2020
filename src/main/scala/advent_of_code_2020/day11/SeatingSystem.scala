package advent_of_code_2020.day11

import advent_of_code_2020.Solution

object SeatingSystem extends Solution[Int] {

  override def day: Int = 11

  override def part1(): Int = {
    occupiedSeatsAtTheEnd(readSeatingArea(inputAsStringLines), 1)
  }

  def printArray(input: Array[Array[Char]]): Unit = {
    for (i <- input(0).indices) {
      for (j <- input.indices) {
        print(s"${input(i)(j)}")
      }
      println()
    }
  }

  override def part2(): Int = occupiedSeatsAtTheEnd(readSeatingArea(inputAsStringLines), 2)

  def occupiedSeatsAtTheEnd(input: Array[Array[Char]], part: Int): Int = {
    var currentSeatMap = input
    val rows           = currentSeatMap.length
    val cols           = currentSeatMap(0).length
    var futureSeatMap  = Array.ofDim[Char](rows, cols)
    var tempSeatMap    = currentSeatMap
    while (!arraysTheSame(futureSeatMap, currentSeatMap)) {
      currentSeatMap = tempSeatMap
      futureSeatMap = Array.ofDim[Char](rows, cols)
      for {
        i <- 0 until rows
        j <- 0 until cols
      } {
        futureSeatMap(i)(j) = seatRule(currentSeatMap, i, j, part)
      }

      tempSeatMap = futureSeatMap
    }
    futureSeatMap.flatten.count(c => c == '#')
  }

  def arraysTheSame(left: Array[Array[Char]], right: Array[Array[Char]]): Boolean = {
    for (i <- left.indices) {
      for (j <- left(0).indices) {
        if (left(i)(j) != right(i)(j)) return false
      }
    }
    true
  }

  def seatRule(seats: Seq[Array[Char]], i: Int, j: Int, part: Int): Char = {
    part match {
      case 1 => nextSeatState(seats, adjacentSeats(seats, i, j), i, j, 4)
      case 2 => nextSeatState(seats, directionSeats(seats, i, j), i, j, 5)
    }
  }

  def nextSeatState(seats: Seq[Array[Char]],
                    neighbourSeats: List[Char],
                    i: Int,
                    j: Int,
                    limit: Int): Char = {
    seats(i)(j) match {
      case 'L' =>
        if (!neighbourSeats.contains('#')) '#' else 'L'
      case '#' =>
        if (neighbourSeats.count(el => el == '#') >= limit) 'L'
        else '#'
      case '.' => '.'
    }
  }

  def adjacentSeats(seats: Seq[Array[Char]], i: Int, j: Int): List[Char] = {
    List((i - 1, j - 1),
         (i, j - 1),
         (i + 1, j - 1),
         (i - 1, j),
         (i + 1, j),
         (i - 1, j + 1),
         (i, j + 1),
         (i + 1, j + 1))
      .filter(el => elementExists(el._1, el._2, seats))
      .map(el => seats(el._1)(el._2))
  }

  def directionSeats(seats: Seq[Array[Char]], i: Int, j: Int): List[Char] = {
    List((-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1))
      .map(el => findFirstSeatTypeInDirection(seats, i, j, el._1, el._2))
      .filter(_.isDefined)
      .map(_.get)
  }

  def findFirstSeatTypeInDirection(seats: Seq[Array[Char]],
                                   i: Int,
                                   j: Int,
                                   iJump: Int,
                                   jJump: Int): Option[Char] = {
    var seat: Option[Char] = None
    var row                = i + iJump
    var col                = j + jJump
    while (elementExists(row, col, seats) && (seat.isEmpty || seat.contains('.'))) {
      seat = Some(seats(row)(col))
      row = row + iJump
      col = col + jJump
    }
    seat
  }

  def elementExists(i: Int, j: Int, seats: Seq[Array[Char]]): Boolean = {
    val rows = seats.length
    val cols = seats.head.length
    i >= 0 && j >= 0 && i < rows && j < cols
  }

  def readSeatingArea(input: Iterator[String]): Array[Array[Char]] = {
    input.map(line => line.toArray).toArray
  }
}
