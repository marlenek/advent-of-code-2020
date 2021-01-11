package advent_of_code_2020.day17

import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class ConwayCubesTest extends FlatSpec with Matchers {

  "conway cubes" should "correctly calculate number of active cubes after 6 cycles in 3 dim array" in {
    val source    = Source.fromURL(getClass.getResource("/input17.txt"))
    val testInput = source.getLines().toList
    InfiniteCube3D.infiniteCube(testInput) should be(112)
    source.close()
  }

  "conway cubes" should "correctly calculate number of active cubes after 6 cycles in 4 dim array" in {
    val source    = Source.fromURL(getClass.getResource("/input17.txt"))
    val testInput = source.getLines().toList
    InfiniteCube4D.infiniteCube(testInput) should be(848)
    source.close()
  }
}
