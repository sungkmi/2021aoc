package lascala.aoc2021.day11_20.day18

import minitest.SimpleTestSuite
import hedgehog.minitest.HedgehogSupport
import hedgehog.*

object Day18Test extends SimpleTestSuite with HedgehogSupport:

  val testInput = """[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"""

  example("day18 - SnailfishNumber#apply") {
    val s = "[[[[[9,8],1],2],3],4]"

    import SnailfishNumber.{RegularNumber => R, Pair => P}

    val expected = P(P(P(P(P(R(9), R(8)), R(1)), R(2)), R(3)), R(4))

    SnailfishNumber(s) ==== expected
  }

  example("day18 - reduce") {
    val reduced = SnailfishNumber("[[[[[9,8],1],2],3],4]").reduce
    reduced.show ==== "[[[[0,9],2],3],4]"
  }

  example("day18 - +") {
    val left = SnailfishNumber("[[[[4,3],4],4],[7,[[8,4],9]]]")
    val right = SnailfishNumber("[1,1]")
    (left + right).show ==== "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
  }

  example("day18 - + #2") {
    val left = SnailfishNumber("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]")
    val right = SnailfishNumber("[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]")
    (left + right).show ==== "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]"
  }

  example("day18 - sum #1") {
    val input = """[1,1]
[2,2]
[3,3]
[4,4]"""
    val expected = "[[[[1,1],[2,2]],[3,3]],[4,4]]"
    sum(input.toNumbers).show ==== expected
  }

  example("day18 - sum #2") {
    val input = """[1,1]
[2,2]
[3,3]
[4,4]
[5,5]"""
    val expected = "[[[[3,0],[5,3]],[4,4]],[5,5]]"
    sum(input.toNumbers).show ==== expected
  }

  example("day18 - sum #3") {
    val input = """[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]"""
    val expected = "[[[[5,0],[7,4]],[5,5]],[6,6]]"
    sum(input.toNumbers).show ==== expected
  }

  example("day18 - sum #4") {
    val input = """[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]"""
    val expected = "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
    sum(input.toNumbers).show ==== expected
  }

  example("day18 - sum #5") {

    val expected = "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]"
    sum(testInput.toNumbers).show ==== expected
  }

  example("day18 - magnitude #1") {
    SnailfishNumber("[[1,2],[[3,4],5]]").magnitude ==== 143
  }

  example("day18 - solve 1") {
    solve1(testInput) ==== 4140
  }

  example("day18 - solve 2") {
    solve2(testInput) ==== 3993
  }

  example("day18 - solve 2 with real input") {
    solve1(input) ==== 4145
  }
  example("day18 - solve 2 with real input") {
    solve2(input) ==== 4855
  }
