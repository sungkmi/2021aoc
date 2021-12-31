package lascala.aoc2021.day1_10.day1

import minitest.SimpleTestSuite
import hedgehog.minitest.HedgehogSupport
import hedgehog.*

object Day1Test extends SimpleTestSuite with HedgehogSupport:
  val depths: Seq[Int] = """199
200
208
210
200
207
240
269
260
263""".split("\n").map(_.toInt).toSeq

  example("day1 - countDeeper") {
    countDeeper(depths) ==== 7
  }

  example("day1 - countDeeperWindows") {
    countDeeperWindows(depths) ==== 5
  }

//  property("reverse alphabetic strings") {
//    for xs <- Gen.alpha.list(Range.linear(0, 100)).forAll
//    yield xs.reverse.reverse ==== xs
//  }
