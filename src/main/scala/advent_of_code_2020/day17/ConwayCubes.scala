package advent_of_code_2020.day17

import advent_of_code_2020.Solution

object ConwayCubes extends Solution[Int] {

  override def day: Int = 17

  override def part1(): Int = InfiniteCube3D.infiniteCube(inputAsStringLines.toList)

  override def part2(): Int = InfiniteCube4D.infiniteCube(inputAsStringLines.toList)
}
