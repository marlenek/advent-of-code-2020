package advent_of_code_2020.day16

case class TicketRule(
    ruleName: String,
    firstRange: (Int, Int),
    secondRange: (Int, Int)
)

object TicketRule {

  def isNumberValidWithAnyRule(ticketRules: IndexedSeq[TicketRule], nr: Int): Boolean = {
    ticketRules.foreach(rule => {
      if ((nr >= rule.firstRange._1 && nr <= rule.firstRange._2) || (nr >= rule.secondRange._1 && nr <= rule.secondRange._2)) {
        return true
      }
    })
    false
  }

  def getValidRulesForNumber(ticketRules: IndexedSeq[TicketRule], nr: Int): List[String] = {
    var rules: List[String] = List.empty
    ticketRules.foreach(rule => {
      if ((nr >= rule.firstRange._1 && nr <= rule.firstRange._2) || (nr >= rule.secondRange._1 && nr <= rule.secondRange._2)) {
        rules = rules :+ rule.ruleName
      }
    })
    rules
  }
}
