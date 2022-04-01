package lascala.aoc2021.day11_20.day16

import minitest.SimpleTestSuite
import hedgehog.minitest.HedgehogSupport
import hedgehog.*

object Day16Test extends SimpleTestSuite with HedgehogSupport:

  example("day16 - decode literal") {
    val input     = "D2FE28"
    val expected  = BITS.Literal(6, BigInt(2021))
    val remainder = "000"
    BITS.decode(input) ==== (expected, remainder)
  }

  example("day16 - decodeBinary #1") {
    val input     = "11010001010"
    val expected  = BITS.Literal(6, BigInt(10))
    val remainder = ""
    BITS.decodeBinary(input) ==== (expected, remainder)
  }

  example("day16 - decodeBinary #2") {
    val input     = "0101001000100100"
    val expected  = BITS.Literal(2, BigInt(20))
    val remainder = ""
    BITS.decodeBinary(input) ==== (expected, remainder)
  }

  example("day16 - decode operator #1") {
    val input = "38006F45291200"
    val expected = BITS.Operator(
      1,
      6,
      Seq(
        BITS.Literal(6, BigInt(10)),
        BITS.Literal(2, BigInt(20)),
      ),
    )
    val remainder = "0000000"
    BITS.decode(input) ==== (expected, remainder)
  }

  example("day16 - decode operator #2") {
    val input = "EE00D40C823060"
    val expected = BITS.Operator(
      7,
      3,
      Seq(
        BITS.Literal(2, BigInt(1)),
        BITS.Literal(4, BigInt(2)),
        BITS.Literal(1, BigInt(3)),
      ),
    )
    val remainder = "00000"
    BITS.decode(input) ==== (expected, remainder)
  }

  example("day16 solve 1 case #1") {
    val input = "8A004A801A8002F478"
    solve1(input) ==== 16
  }

  example("day16 solve 1 case #2") {
    val input = "620080001611562C8802118E34"
    solve1(input) ==== 12
  }

  example("day16 solve 1 case #3") {
    val input = "C0015000016115A2E0802F182340"
    solve1(input) ==== 23
  }

  example("day16 solve 1 case #4") {
    val input = "A0016C880162017C3686B18A3D4780"
    solve1(input) ==== 31
  }

  example("day16 solve 2 case #1") {
    val input = "C200B40A82"
    solve2(input) ==== 3
  }

  example("day16 solve 2 case #2") {
    val input = "04005AC33890"
    solve2(input) ==== 54
  }

  example("day16 solve 2 case #3") {
    val input = "880086C3E88112"
    solve2(input) ==== 7
  }

  example("day16 solve 2 case #4") {
    val input = "CE00C43D881120"
    solve2(input) ==== 9
  }

  example("day16 solve 2 case #5") {
    val input = "D8005AC2A8F0"
    solve2(input) ==== 1
  }

  example("day16 solve 2 case #6") {
    val input = "F600BC2D8F"
    solve2(input) ==== 0
  }

  example("day16 solve 2 case #7") {
    val input = "9C005AC2F8F0"
    solve2(input) ==== 0
  }

  example("day16 solve 2 case #8") {
    val input = "9C0141080250320F1802104A08"
    solve2(input) ==== 1
  }

  example("day16 solve1 with real input") {
    solve1(input) ==== 920
  }

  example("day16 solve2 with real input") {
    solve2(input) ==== BigInt("10185143721112")
  }

