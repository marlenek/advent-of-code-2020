package advent_of_code_2020.day13

import advent_of_code_2020.day13.ShuttleSearch.{
  findMeetingTsBruteForce,
  findMeetingTsChineseRemainderTheorem,
  findNextBus
}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class ShuttleSearchTest extends FlatSpec with Matchers {

  "shuttle search" should "find the best suiting bus for given ts" in {
    val source    = Source.fromURL(getClass.getResource("/input13.txt"))
    val testInput = source.getLines().toArray
    findNextBus(testInput) should be(295)
    source.close()
  }

  "shuttle search" should "find matching point for all the buses example 1" in {
    findMeetingTsBruteForce("7,13,x,x,59,x,31,19") should be(1068781)
    findMeetingTsChineseRemainderTheorem("7,13,x,x,59,x,31,19") should be(1068781)
  }

  "shuttle search" should "find matching point for all the buses example 2" in {
    findMeetingTsBruteForce("17,x,13,19") should be(3417)
    findMeetingTsChineseRemainderTheorem("17,x,13,19") should be(3417)
  }

  "shuttle search" should "find matching point for all the buses example 3" in {
    findMeetingTsBruteForce("67,7,59,61") should be(754018)
    findMeetingTsChineseRemainderTheorem("67,7,59,61") should be(754018)
  }

  "shuttle search" should "find matching point for all the buses example 4" in {
    findMeetingTsBruteForce("67,x,7,59,61") should be(779210)
    findMeetingTsChineseRemainderTheorem("67,x,7,59,61") should be(779210)
  }

  "shuttle search" should "find matching point for all the buses example 5" in {
    findMeetingTsBruteForce("67,7,x,59,61") should be(1261476)
    findMeetingTsChineseRemainderTheorem("67,7,x,59,61") should be(1261476)
  }

  "shuttle search" should "find matching point for all the buses example 6" in {
    findMeetingTsBruteForce("1789,37,47,1889") should be(1202161486)
    findMeetingTsChineseRemainderTheorem("1789,37,47,1889") should be(1202161486)
  }

}
