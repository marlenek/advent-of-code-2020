package advent_of_code_2020.day16

import advent_of_code_2020.day16.InputReader.readRules
import advent_of_code_2020.day16.TicketTranslation.ticketScanningErrorRate
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class TicketTranslationTest extends FlatSpec with Matchers {

  "ticket translation" should "correctly create mask numbers and sum" in {
    val source    = Source.fromURL(getClass.getResource("/input16.txt"))
    val testInput = source.getLines().toList
    readRules(testInput).foreach(rule => println(rule))
    ticketScanningErrorRate(testInput) should be(71)
    source.close()
  }

}
