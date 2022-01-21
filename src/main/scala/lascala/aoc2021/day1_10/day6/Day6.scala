package lascala.aoc2021.day1_10.day6

opaque type School = Vector[BigInt]

extension (s: School)
  def next: School =
    val (head, tail) = s.splitAt(1)
    tail.updated(6, tail(6) + head(0)) :+ head(0)
  end next

  def total: BigInt = s.sum

  def day(n: Int): School = (0 until n).foldLeft(s)((school, _) => school.next)
end extension

object School:
  def empty: School = Vector.fill(9)(BigInt(0))

  def parse(s: String): School = s.split(",").map(_.toInt).foldLeft(empty) {
    case (school, digit) => school.updated(digit, school(digit) + 1)
  }
end School

def solve1(s: String): BigInt =
  School.parse(s).day(80).total
end solve1

def solve2(s: String): BigInt =
  School.parse(s).day(256).total
end solve2

@main def part1: Unit =
  val ans = solve1(input)
  println(ans)

@main def part2: Unit =
  val ans = solve2(input)
  println(ans)

val input =
  """1,2,5,1,1,4,1,5,5,5,3,4,1,2,2,5,3,5,1,3,4,1,5,2,5,1,4,1,2,2,1,5,1,1,1,2,4,3,4,2,2,4,5,4,1,2,3,5,3,4,1,1,2,2,1,3,3,2,3,2,1,2,2,3,1,1,2,5,1,2,1,1,3,1,1,5,5,4,1,1,5,1,4,3,5,1,3,3,1,1,5,2,1,2,4,4,5,5,4,4,5,4,3,5,5,1,3,5,2,4,1,1,2,2,2,4,1,2,1,5,1,3,1,1,1,2,1,2,2,1,3,3,5,3,4,2,1,5,2,1,4,1,1,5,1,1,5,4,4,1,4,2,3,5,2,5,5,2,2,4,4,1,1,1,4,4,1,3,5,4,2,5,5,4,4,2,2,3,2,1,3,4,1,5,1,4,5,2,4,5,1,3,4,1,4,3,3,1,1,3,2,1,5,5,3,1,1,2,4,5,3,1,1,1,2,5,2,4,5,1,3,2,4,5,5,1,2,3,4,4,1,4,1,1,3,3,5,1,2,5,1,2,5,4,1,1,3,2,1,1,1,3,5,1,3,2,4,3,5,4,1,1,5,3,4,2,3,1,1,4,2,1,2,2,1,1,4,3,1,1,3,5,2,1,3,2,1,1,1,2,1,1,5,1,1,2,5,1,1,4"""
