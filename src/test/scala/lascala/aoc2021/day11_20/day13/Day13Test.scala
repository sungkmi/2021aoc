package lascala.aoc2021.day11_20.day13

import minitest.SimpleTestSuite
import hedgehog.minitest.HedgehogSupport
import hedgehog.*

object Day13Test extends SimpleTestSuite with HedgehogSupport:

  val testInput = """6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"""

  example("day13 - solve 1") {
    solve1(testInput) ==== 17
  }

  example("day13 - solve 2") {
    val expected = """#####
#...#
#...#
#...#
#####"""

    solve2(testInput) ==== expected
  }
