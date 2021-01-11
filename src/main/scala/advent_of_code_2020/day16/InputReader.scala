package advent_of_code_2020.day16

object InputReader {

  def readRules(input: List[String]): IndexedSeq[TicketRule] = {
    val rulesSize = input.slice(0, input.indexOf("")).length
    for (i <- 0 until rulesSize) yield {
      val ranges = input(i).split(':')(1).split(Array(' ', '-'))
      TicketRule(
        input(i).split(':')(0),
        (ranges(1).toInt, ranges(2).toInt),
        (ranges(4).toInt, ranges(5).toInt)
      )
    }
  }

  def readMyTicket(input: List[String]): Array[Int] = {
    val nearByTicketsStartingIndex = input.indexOf("your ticket:")
    input(nearByTicketsStartingIndex + 1).split(',').map(_.toInt)
  }

  def readNearbyTickets(input: List[String]): IndexedSeq[Array[Int]] = {
    val nearByTicketsStartingIndex = input.indexOf("nearby tickets:")
    for (i <- nearByTicketsStartingIndex + 1 until input.length) yield {
      input(i).split(',').map(_.toInt)
    }
  }

}
