package advent_of_code_2020.day10

import advent_of_code_2020.day10.AdapterArray.{calcAdapterMultiDiff, calcVariations}
import org.scalatest.{FlatSpec, Matchers}

class AdapterArrayTest extends FlatSpec with Matchers {

  val firstSeq = Seq(16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4)

  val secondSeq = Seq(28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1,
    32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3)

  "adapter array" should "have correct number of adapters for first example" in {
    calcAdapterMultiDiff(firstSeq) should be(35)
  }

  "adapter array" should "have correct number of adapters for second example" in {
    calcAdapterMultiDiff(secondSeq) should be(220)
  }

  "adapter array" should "have correct number of arrangements for first example" in {
    calcVariations(firstSeq) should be(8)
  }

  "adapter array" should "have correct number of arrangements for second example" in {
    calcVariations(secondSeq) should be(19208)
  }
}
