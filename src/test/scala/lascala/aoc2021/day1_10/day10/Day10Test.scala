package lascala.aoc2021.day1_10.day10

import minitest.SimpleTestSuite
import hedgehog.minitest.HedgehogSupport
import hedgehog.*

object Day10Test extends SimpleTestSuite with HedgehogSupport:

  val testInput = """[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]"""

  example("day10 - check") {
    Result.all(List(
      Chunk("()").check ==== Right(Chunk.empty),
      Chunk("{([(<{}[<>[]}>{[]{[(<()>").check ==== Left('}'),
      Chunk("[[<[([]))<([[{}[[()]]]").check ==== Left(')'),
      Chunk("[{[{({}]{}}([{[{{{}}([]").check ==== Left(']'),
      Chunk("[<(<(<(<{}))><([]([]()").check ==== Left(')'),
      Chunk("<{([([[(<>()){}]>(<<{{").check ==== Left('>'),
    ))
  }

  example("day10 - solve 1") {
    solve1(testInput) ==== 26397
  }

  example("day10 - solve 2") {
    solve2(testInput) ==== 288957
  }

end Day10Test
