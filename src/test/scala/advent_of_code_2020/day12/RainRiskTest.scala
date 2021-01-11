package advent_of_code_2020.day12

import advent_of_code_2020.day12.RainRisk.{ferryNavigation, waypointFerryNavigation}
import org.scalatest.{FlatSpec, Matchers}

class RainRiskTest extends FlatSpec with Matchers {

  "rain risk" should "manhattan distance should be 25" in {
    val testInput = Array("F10", "N3", "F7", "R90", "F11")
    ferryNavigation(testInput) should be(25)
  }

  "rain risk" should "manhattan distance in waypoint navigation should be 286" in {
    val testInput = Array("F10", "N3", "F7", "R90", "F11")
    waypointFerryNavigation(testInput) should be(286)
  }


}
