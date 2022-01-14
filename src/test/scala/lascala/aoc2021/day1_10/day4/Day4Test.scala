package lascala.aoc2021.day1_10.day4

import minitest.SimpleTestSuite
import hedgehog.minitest.HedgehogSupport
import hedgehog.*

object Day4Test extends SimpleTestSuite with HedgehogSupport:

  val testInput = """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7"""


  example("day4 - bingo board") {

    val boardString = """22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19"""

    val board = BingoBoard.parse(boardString)

    Result.all(
      List(
        board.indexOf(0) ==== Some((0, 4)),
        board.indexOf(1) ==== Some((4, 0)),
        board.indexOf(2) ==== Some((1, 1)),
      ),
    )
  }

  example("day4 - solve1") {
    solve1(testInput) ==== 4512
  }

  example("day4 - solve2") {
    solve2(testInput) ==== 1924
  }

end Day4Test
