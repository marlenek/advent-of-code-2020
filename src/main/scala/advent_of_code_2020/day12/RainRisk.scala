package advent_of_code_2020.day12

import advent_of_code_2020.Solution

import scala.annotation.tailrec

object RainRisk extends Solution[Int] {

  override def day: Int = 12

  override def part1(): Int = ferryNavigation(inputAsStringLines.toArray)

  def ferryNavigation(input: Array[String]): Int = {
    var x         = 0
    var y         = 0
    var direction = 'E'
    input.foreach(line => {
      val action = line.charAt(0)
      val value  = line.substring(1).toInt
      action match {
        case v if v == 'N' || v == 'S' || v == 'E' || v == 'W' =>
          x = move(action, value, x, y)._1
          y = move(action, value, x, y)._2
        case v if v == 'L' || v == 'R' => direction = rotate(action, value, direction)
        case 'F' =>
          x = move(direction, value, x, y)._1
          y = move(direction, value, x, y)._2
      }
    })
    Math.abs(x) + Math.abs(y)
  }

  def move(action: Char, value: Int, x: Int, y: Int): (Int, Int) = {
    action match {
      case 'N' => (x, y + value)
      case 'S' => (x, y - value)
      case 'E' => (x + value, y)
      case 'W' => (x - value, y)
    }
  }

  def moveToWaypoint(value: Int, ferry: Int, waypoint: Int): Int = {
    ferry + (waypoint * value)
  }

  @tailrec
  def rotateWaypoint(action: Char, value: Int, x: Int, y: Int): (Int, Int) = {
    action match {
      case 'R' =>
        value match {
          case 90  => (y, -x)
          case 180 => (-x, -y)
          case 270 => (-y, x)
        }
      case 'L' => rotateWaypoint('R', 360 - value, x, y)
    }
  }

  @tailrec
  def rotate(action: Char, value: Int, direction: Char): Char = {
    val worldDirections = Array('E', 'S', 'W', 'N')
    action match {
      case 'R' => worldDirections((worldDirections.indexOf(direction) + value / 90) % 4)
      case 'L' => rotate('R', 360 - value, direction)
    }
  }

  def waypointFerryNavigation(input: Array[String]): Int = {
    var xFerry    = 0
    var yFerry    = 0
    var xWaypoint = 10
    var yWaypoint = 1
    input.foreach(line => {
      val action = line.charAt(0)
      val value  = line.substring(1).toInt
      action match {
        case v if v == 'N' || v == 'S' || v == 'E' || v == 'W' =>
          val res = move(action, value, xWaypoint, yWaypoint)
          xWaypoint = res._1
          yWaypoint = res._2
        case v if v == 'L' || v == 'R' =>
          val res = rotateWaypoint(action, value, xWaypoint, yWaypoint)
          xWaypoint = res._1
          yWaypoint = res._2
        case 'F' =>
          xFerry = moveToWaypoint(value, xFerry, xWaypoint)
          yFerry = moveToWaypoint(value, yFerry, yWaypoint)
      }
    })
    Math.abs(xFerry) + Math.abs(yFerry)
  }

  override def part2(): Int = waypointFerryNavigation(inputAsStringLines.toArray)
}
