package lascala.aoc2021.day11_20.day12

import minitest.SimpleTestSuite
import hedgehog.minitest.HedgehogSupport
import hedgehog.*

object Day12Test extends SimpleTestSuite with HedgehogSupport:

  val testInput1 = """start-A
start-b
A-c
A-b
b-d
A-end
b-end"""

  val testInput2 = """dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc"""

  val testInput3 = """fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW"""

  example("day12 - solve 1 with test input #1") {
    solve1(testInput1) ==== 10
  }

  example("day12 - solve 1 with test input #2") {
    solve1(testInput2) ==== 19
  }
  
  example("day12 - solve 1 with test input #3") {
    solve1(testInput3) ==== 226
  }

  example("day12 - solve 2 with test input #1") {
    solve2(testInput1) ==== 36
  }

  example("day12 - solve 2 with test input #2") {
    solve2(testInput2) ==== 103
  }
  
  example("day12 - solve 2 with test input #3") {
    solve2(testInput3) ==== 3509
  }
end Day12Test
