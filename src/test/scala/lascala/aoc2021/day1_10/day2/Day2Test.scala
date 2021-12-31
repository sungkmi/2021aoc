package lascala.aoc2021.day1_10.day2

import minitest.SimpleTestSuite
import hedgehog.minitest.HedgehogSupport
import hedgehog.*

object Day2Test extends SimpleTestSuite with HedgehogSupport:
  val exampleInput: String = """forward 5
down 5
forward 8
up 3
down 8
forward 2"""

  example("day2 - solve1") {
    solve1(exampleInput) ==== 150
  }

  example("day2 - solve2") {
    solve2(exampleInput) ==== 900
  }
