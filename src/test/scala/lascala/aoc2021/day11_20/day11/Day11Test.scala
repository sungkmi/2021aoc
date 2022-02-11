package lascala.aoc2021.day11_20.day11

import minitest.SimpleTestSuite
import hedgehog.minitest.HedgehogSupport
import hedgehog.*

object Day11Test extends SimpleTestSuite with HedgehogSupport:

  val testInput = """5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526"""

  example("day11 - step") {
    val energyLevel = EnergyLevel.parse("""11111
19991
19191
19991
11111""")

    val expected = EnergyLevel.parse("""34543
40004
50005
40004
34543""")

    energyLevel.step ==== (expected, 9)
  }

  example("day11 - step #2") {
    val energyLevel = EnergyLevel.parse(testInput)

    val expected = EnergyLevel.parse("""6594254334
3856965822
6375667284
7252447257
7468496589
5278635756
3287952832
7993992245
5957959665
6394862637""")

    energyLevel.step ==== (expected, 0)
  }

  example("day11 - solve 1") {
    solve1(testInput) ==== 1656
  }

  example("day11 - solve 2") {
    solve2(testInput) ==== 195
  }
  
end Day11Test
