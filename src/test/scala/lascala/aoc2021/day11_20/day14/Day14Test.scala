package lascala.aoc2021.day11_20.day14

import minitest.SimpleTestSuite
import hedgehog.minitest.HedgehogSupport
import hedgehog.*

object Day14Test extends SimpleTestSuite with HedgehogSupport:

  val testInput = """NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"""

  example("day14 - solve 1") {
    solve1(testInput) ==== 1588
  }

  example("day14 - solve 2") {
    solve2(testInput) ==== BigInt("2188189693529")
  }
