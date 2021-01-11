package advent_of_code_2020.day5

case class BoardingCard(
    seatCode: String
)

object BoardingCard {

  def calculateRowAndColumn(boardingCard: BoardingCard): (Int, Int) = {
    var minColumn = 0
    var maxColumn = 7
    var minRow    = 0
    var maxRow    = 127
    boardingCard.seatCode.toCharArray.foreach {
      case 'F' => maxRow = splitByTwo(minRow, maxRow)
      case 'B' => minRow = splitByTwo(minRow, maxRow)
      case 'L' => maxColumn = splitByTwo(minColumn, maxColumn)
      case 'R' => minColumn = splitByTwo(minColumn, maxColumn)
    }
    (minRow, minColumn)
  }

  def calculateSeatNumber(boardingCard: BoardingCard): Int = {
    calculateRowAndColumn(boardingCard)._1 * 8 + calculateRowAndColumn(boardingCard)._2
  }

  def splitByTwo(min: Int, max: Int): Int = {
    (min + 1 + max) / 2
  }
}
