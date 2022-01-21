package lascala.aoc2021.day1_10.day7

import minitest.SimpleTestSuite
import hedgehog.minitest.HedgehogSupport
import hedgehog.*

object Day7Test extends SimpleTestSuite with HedgehogSupport:

  val testInput = """16,1,2,0,4,2,7,1,2,14"""

  example("day7 - solve1") {
    solve1(testInput) ==== 37
  }

  example("day7 - solve2") {
    solve2(testInput) ==== 168
  }

end Day7Test
