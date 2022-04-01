package lascala.aoc2021.day11_20.day17

import minitest.SimpleTestSuite
import hedgehog.minitest.HedgehogSupport
import hedgehog.*

object Day17Test extends SimpleTestSuite with HedgehogSupport:

  val testInput = """target area: x=20..30, y=-10..-5"""

  example("day17 - TargetArea#parse") {
    TargetArea.parse(testInput) ==== TargetArea(20, 30, -10, -5)
  }

  example("day17 - TargetArea#highestY case #1") {
    val targetArea = TargetArea.parse(testInput)
    targetArea.highestY(7, 2) ==== Some(3)
  }

  example("day17 - TargetArea#highestY case #2") {
    val targetArea = TargetArea.parse(testInput)
    targetArea.highestY(6, 3) ==== Some(6)
  }

  example("day17 - TargetArea#highestY case #3") {
    val targetArea = TargetArea.parse(testInput)
    targetArea.highestY(9, 0) ==== Some(0)
  }

  example("day17 - TargetArea#highestY case #4") {
    val targetArea = TargetArea.parse(testInput)
    targetArea.highestY(17, -4) ==== None
  }

  example("day17 - TargetArea#highestY case #5") {
    val targetArea = TargetArea.parse(testInput)
    targetArea.highestY(6, 9) ==== Some(45)
  }

  example("day17 - solve 1") {
    solve1(testInput) ==== 45
  }

  example("day17 - solve 2") {
    solve2(testInput) ==== 112
  }

  example("day17 - solve 1 with real input") {
    solve1(input) ==== 3160
  }

  example("day17 - solve2 with real input") {
    solve2(input) ==== 1928
  }
