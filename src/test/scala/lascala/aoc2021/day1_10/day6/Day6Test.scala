package lascala.aoc2021.day1_10.day6

import minitest.SimpleTestSuite
import hedgehog.minitest.HedgehogSupport
import hedgehog.*

object Day6Test extends SimpleTestSuite with HedgehogSupport:

  val testInput = """3,4,3,1,2"""

  example("day6 - solve1") {
    solve1(testInput) ==== 5934
  }

  example("day6 - solve2") {
    solve2(testInput) ==== BigInt("26984457539")
  }
end Day6Test
