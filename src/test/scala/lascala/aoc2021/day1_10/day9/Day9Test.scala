package lascala.aoc2021.day1_10.day9

import minitest.SimpleTestSuite
import hedgehog.minitest.HedgehogSupport
import hedgehog.*

object Day9Test extends SimpleTestSuite with HedgehogSupport:

  val testInput = """2199943210
3987894921
9856789892
8767896789
9899965678"""

  example("day9 - solve1") {
    solve1(testInput) ==== 15
  }
  
  example("day9 - solve2") {
    solve2(testInput) ==== 1134
  }

end Day9Test
