package advent_of_code_2020.day13

import advent_of_code_2020.Solution

object ShuttleSearch extends Solution[Long] {

  override def day: Int = 13

  override def part1(): Long = {
    findNextBus(inputAsStringLines.toArray)
  }

  def findNextBus(input: Array[String]): Int = {
    val ts    = input(0).toInt
    val buses = input(1).split(',').filterNot(_ == "x").map(_.toInt)
    val perfectBus = buses
      .map(bus => (bus, nextBusTs(bus, ts)))
      .minBy(_._2)
    perfectBus._1 * (perfectBus._2 - ts)
  }

  def nextBusTs(bus: Int, ts: Int): Int = {
    val product: Int = ts / bus
    product * bus + bus
  }

  override def part2(): Long = {
    findMeetingTsChineseRemainderTheorem(inputAsStringLines.toArray.tail.head)
  }

  // thanks to https://github.com/caderek/aoc2020/blob/main/src/day13/README.md
  def findMeetingTsChineseRemainderTheorem(input: String): Long = {
    val buses                                 = input.split(',')
    var allIndexedBuses: Array[(Int, String)] = Array.empty
    for (i <- buses.indices) {
      allIndexedBuses = allIndexedBuses :+ (i, buses(i))
    }
    val busRemainder =
      allIndexedBuses
        .filter(bus => bus._2 != "x")
        .map(bus => (bus._2.toLong, bus._1.toLong))
        .map(
          bus =>
            (bus._1,
             if (bus._2 == 0) 0
             else if (bus._2 > bus._1)
               bus._1 - (bus._2 % bus._1)
             else bus._1 - bus._2))

    val eea = busRemainder.map(busRem => {
      val busNrProduct = busRemainder.map(b => b._1).product / busRem._1
      val gcd          = gcdExtended(busNrProduct, busRem._1)._2
      busRem._2 * busNrProduct * gcd
    })
    val sumEea       = eea.sum
    val busNrProduct = busRemainder.map(b => b._1).product
    val mod          = sumEea % busNrProduct
    if (mod < 0) mod + busNrProduct else mod
  }

  def gcdExtended(a: Long, b: Long): (Long, Long) = {
    var x  = 1L
    var y  = 0L
    var r  = 0L
    var s  = 1L
    var a1 = a
    var b1 = b

    while (b1 != 0) {
      val c = a1 % b1
      val q = a1 / b1
      a1 = b1
      b1 = c
      val rPrim = r
      val sPrim = s
      r = x - q * r
      s = y - q * s
      x = rPrim
      y = sPrim
    }
    (a1, x)
  }

  def findMeetingTsBruteForce(input: String): Long = {
    val buses                                 = input.split(',')
    var allIndexedBuses: Array[(Int, String)] = Array.empty
    for (i <- buses.indices) {
      allIndexedBuses = allIndexedBuses :+ (i, buses(i))
    }
    val indexedBuses =
      allIndexedBuses.filter(bus => bus._2 != "x").map(bus => (bus._1, bus._2.toInt))
    val maxTsBus         = indexedBuses.maxBy(_._2)._2.toLong
    val maxTsBusIndex    = indexedBuses.maxBy(_._2)._1.toLong
    var perfectTimestamp = (100000000000000L / maxTsBus) * maxTsBus
    var flag             = false
    var i                = 1
    while (!flag) {
      flag = true
      perfectTimestamp = (i * maxTsBus) - maxTsBusIndex
      flag = checkBuses(indexedBuses, perfectTimestamp)
      i = i + 1
    }
    perfectTimestamp
  }

  def checkBuses(buses: Array[(Int, Int)], perfectTimestamp: Long): Boolean = {
    buses.foreach(
      bus =>
        while ((perfectTimestamp + bus._1) % bus._2 != 0) {
          return false
      }
    )
    true
  }

}
