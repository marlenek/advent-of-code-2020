package advent_of_code_2020.day16

import advent_of_code_2020.Solution
import advent_of_code_2020.day16.InputReader.{readMyTicket, readNearbyTickets, readRules}

object TicketTranslation extends Solution[Long] {

  override def day: Int = 16

  override def part1(): Long = {
    val input = inputAsStringLines.toList
    ticketScanningErrorRate(input)
  }

  def ticketScanningErrorRate(input: List[String]): Int = {
    val rules   = readRules(input)
    val numbers = readNearbyTickets(input)
    numbers.flatten.filter(nr => !TicketRule.isNumberValidWithAnyRule(rules, nr)).sum
  }

  override def part2(): Long = {
    val input = inputAsStringLines.toList
    departureFieldsProduct(input)
  }

  def departureFieldsProduct(input: List[String]): Long = {

    val rules         = readRules(input)
    val numbers       = readNearbyTickets(input)
    val validTickets  = filterOnlyValidTickets(numbers, rules)
    val ticketToRules = mapTicketToRules(validTickets, rules)

    val ticketToRulesFlatten = for (i <- rules.indices)
      yield ticketToRules.flatMap(rule => rule(i)).toList

    val matchingRules = ticketToRulesFlatten.map(t => {
      t.groupBy(identity)
        .view
        .mapValues(_.size)
        .filter(f => f._2 == validTickets.length)
    })
    var finalRules: List[(String, Int)] = List.empty
    while (finalRules.length != rules.length) {
      for (i <- rules.indices) {
        val filteredRow = matchingRules(i).filterNot(el => finalRules.map(_._1).contains(el._1))
        if (filteredRow.size == 1)
          finalRules = finalRules :+ (filteredRow.head._1, i)
      }
    }
    val myTicket         = readMyTicket(input)
    val departureIndexes = finalRules.filter(p => p._1.startsWith("departure")).map(_._2)
    departureIndexes.map(i => myTicket(i)).map(_.toLong).product
  }

  def filterOnlyValidTickets(tickets: IndexedSeq[Array[Int]],
                             rules: IndexedSeq[TicketRule]): IndexedSeq[Array[Int]] = {
    tickets.filterNot(ticket => {
      ticket.exists(nr => !TicketRule.isNumberValidWithAnyRule(rules, nr))
    })
  }

  def mapTicketToRules(tickets: IndexedSeq[Array[Int]],
                       rules: IndexedSeq[TicketRule]): IndexedSeq[Array[List[String]]] = {
    tickets.map(ticket => {
      ticket.map(nr => TicketRule.getValidRulesForNumber(rules, nr))
    })
  }

}
