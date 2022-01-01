package lascala.aoc2021.day1_10.day3

import minitest.SimpleTestSuite
import hedgehog.minitest.HedgehogSupport
import hedgehog.*

object Day3Test extends SimpleTestSuite with HedgehogSupport:
  val exampleInput: String = """00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010"""

  example("day3 - gamma") {
    val d = DiagnosticReport.parse(exampleInput)
    d.gamma.toString(2) ==== "10110"
  }

  example("day3 - epsilon") {
    val d = DiagnosticReport.parse(exampleInput)
    d.epsilon ==== 9
  }

  example("day3 - oxygen") {
    val d = DiagnosticReport.parse(exampleInput)
    d.oxygen ==== 23
  }

  example("day3 - co2") {
    val d = DiagnosticReport.parse(exampleInput)
    d.co2 ==== 10
  }

  example("day3 - solve1") {
    solve1(exampleInput) ==== 198
  }

  example("day3 - solve2") {
    solve2(exampleInput) ==== 230
  }
