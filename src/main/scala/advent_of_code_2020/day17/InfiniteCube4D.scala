package advent_of_code_2020.day17

object InfiniteCube4D {

  def infiniteCube(input: List[String]): Int = {
    var cube = initializeCube(input.toArray)
    val neighbours = List
      .fill(4)(-1 to 1)
      .flatten
      .combinations(4)
      .flatMap(_.permutations)
      .toList
      .filterNot(l => l == List(0, 0, 0, 0))
    val x, y, z, w: Int = cube.length

    for (_ <- 1 to 6) {
      var activePointsInNextIter: List[(Int, Int, Int, Int)] = List.empty
      for {
        i <- 1 until x - 1
        j <- 1 until y - 1
        k <- 1 until z - 1
        l <- 1 until w - 1
      } {
        val currentState = cube(i)(j)(k)(l)
        val activeNeighbours = neighbours
          .map(n => {
            cube(n(0) + i)(n(1) + j)(n(2) + k)(n(3) + l)
          })
          .sum
        if (currentState == 1 && (activeNeighbours == 2 || activeNeighbours == 3) || (currentState == 0 && activeNeighbours == 3)) {
          activePointsInNextIter = activePointsInNextIter :+ (i, j, k, l)
        }
      }
      cube = fillCube(cube, activePointsInNextIter)
    }
    cube.flatten.flatten.flatten.sum
  }

  def initializeCube(input: Array[String]): Array[Array[Array[Array[Int]]]] = {
    val steps                    = 6
    val initialSliceSize         = input.length
    val cubeSize                 = initialSliceSize + (steps * 2) + 2
    val initializationStartPoint = steps + 1
    val x, y, z, w: Int          = cubeSize
    val cube                     = Array.ofDim[Int](x, y, z, w)
    for {
      i <- initializationStartPoint until (initializationStartPoint + initialSliceSize)
      j <- initializationStartPoint until (initializationStartPoint + initialSliceSize)
    } {
      if (input(i - initializationStartPoint).charAt(j - initializationStartPoint) == '#')
        cube(i)(j)(initializationStartPoint)(initializationStartPoint) = 1
    }
    cube
  }

  def fillCube(cube: Array[Array[Array[Array[Int]]]],
               activePoints: List[(Int, Int, Int, Int)]): Array[Array[Array[Array[Int]]]] = {
    val cubeSize        = cube.length
    val x, y, z, w: Int = cubeSize
    for {
      i <- 0 until x
      j <- 0 until y
      k <- 0 until z
      l <- 0 until w
    } {
      if (activePoints.contains((i, j, k, l)))
        cube(i)(j)(k)(l) = 1
      else cube(i)(j)(k)(l) = 0
    }
    cube
  }
}
