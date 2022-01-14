package lascala.aoc2021.day1_10.day5

import minitest.SimpleTestSuite
import hedgehog.minitest.HedgehogSupport
import hedgehog.*

object Day5Test extends SimpleTestSuite with HedgehogSupport:

  val testInput = """0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"""

  example("day5 - solve1") {
    solve1(testInput) ==== 5
  }

  example("day5 - solve2") {
    solve2(testInput) ==== 12
  }

end Day5Test
