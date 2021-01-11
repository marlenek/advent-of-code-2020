package advent_of_code_2020.day14

import advent_of_code_2020.Solution

object DockingData extends Solution[Long] {

  override def day: Int = 14

  override def part1(): Long = initializationProgram(inputAsStringLines.toArray, 1)

  def initializationProgram(input: Array[String], part: Int): Long = {
    var memory       = Map[Long, Long]()
    var mask: String = ""

    input.foreach(line => {
      if (line.startsWith("mask")) {
        mask = line.substring(line.lastIndexOf(" ") + 1)
      } else if (line.startsWith("mem")) {
        val memAddress = line.split(Array('[', ']'))(1).toLong
        val value      = line.substring(line.lastIndexOf(" ") + 1).toInt
        part match {
          case 1 =>
            val newValue = maskValue(mask.toCharArray, value)
            memory = memory + (memAddress -> newValue)
          case 2 =>
            val newAddresses = maskAddress(mask.toCharArray, memAddress)
            newAddresses.foreach(address => memory = memory + (address -> value))
        }
      }
    })
    memory.values.sum
  }

  def maskValue(mask: Array[Char], value: Int): Long = {
    val numberAs36Bits: Array[Char] = maskNumberWithMask(mask, value, List('0', '1'))
    _0b(numberAs36Bits.mkString)
  }

  def maskAddress(mask: Array[Char], memoryAddress: Long): List[Long] = {
    val numberAs36Bits: Array[Char] = maskNumberWithMask(mask, memoryAddress, List('X', '1'))
    val xs                          = numberAs36Bits.count(_ == 'X')
    val permutations                = List.fill(xs)('0' to '1').flatten.combinations(xs).flatMap(_.permutations)
    val xIndexes                    = numberAs36Bits.zipWithIndex.filter(item => item._1 == 'X').map(_._2)
    val opt = permutations.map(permutation => {
      val address = numberAs36Bits.clone()
      for (i <- 0 until xs) {
        address(xIndexes(i)) = permutation(i)
      }
      _0b(address.mkString)
    })
    opt.toList
  }

  def _0b(row: String): Long = {
    row.reverse
      .split("")
      .zipWithIndex
      .map(x => (x._1.toInt, x._2))
      .filter(_._1 == 1)
      .map(x => Math.pow(2, x._2).toLong)
      .sum
  }

  def maskNumberWithMask(mask: Array[Char],
                         memoryAddress: Long,
                         charsToMask: List[Char]): Array[Char] = {
    val numberAsBits = memoryAddress.toBinaryString.toCharArray
    val length       = 36 - numberAsBits.length
    val numberAs36Bits: Array[Char] = Array.fill(length) {
      '0'
    } :++ numberAsBits
    for (i <- 0 to 35) {
      if (charsToMask.contains(mask(i)))
        numberAs36Bits(i) = mask(i)
    }
    numberAs36Bits
  }

  override def part2(): Long = initializationProgram(inputAsStringLines.toArray, 2)
}
