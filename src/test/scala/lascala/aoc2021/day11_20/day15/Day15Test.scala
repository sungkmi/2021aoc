package lascala.aoc2021.day11_20.day15

import minitest.SimpleTestSuite
import hedgehog.minitest.HedgehogSupport
import hedgehog.*

object Day15Test extends SimpleTestSuite with HedgehogSupport:

  val testInput = """1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"""

  example("day15 - solve 1") {
    solve1(testInput) ==== 40
  }

  example("day15 - solve 2") {
    solve2(testInput) ==== 315
  }

  example("day15 - solve 1 - real") {
    val s = """11199
99199
91199
91999
91111"""
    solve1(s) ==== 10
  }
