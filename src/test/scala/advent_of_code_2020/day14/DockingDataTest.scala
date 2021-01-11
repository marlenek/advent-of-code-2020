package advent_of_code_2020.day14

import advent_of_code_2020.day14.DockingData.{_0b, initializationProgram, maskValue}
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source

class DockingDataTest  extends FlatSpec with Matchers {

  "docking data" should "correctly create mask numbers and sum" in {
    val source = Source.fromURL(getClass.getResource("/input14.txt"))
    val testInput = source.getLines().toArray
    initializationProgram(testInput, 1) should be(165)
    source.close()
  }

  "docking data" should "correctly mask number" in {
    val mask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
    maskValue(mask.toCharArray, 11) should be(73)
  }

  "docking data" should "correctly parse binary string" in {
      _0b("000100100010101011100000000011110111") should be(4876796151L)
  }

  "docking data" should "correctly calculate part 2" in {
    val source = Source.fromURL(getClass.getResource("/input14_2.txt"))
    val testInput = source.getLines().toArray
    initializationProgram(testInput, 2) should be(208)
    source.close()
  }


}