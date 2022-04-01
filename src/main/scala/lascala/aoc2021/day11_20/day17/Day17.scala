package lascala.aoc2021.day11_20.day17

case class TargetArea(x0: Int, x1: Int, y0: Int, y1: Int)

object TargetArea:
  def parse(s: String): TargetArea =
    val prefix                = "target area: "
    val Array(xRange, yRange) = s.drop(prefix.size).split(", ").map(_.drop(2))
    def parseRange(s: String): (Int, Int) =
      val Array(x0, x1) = s.split("\\.\\.")
      (x0.toInt, x1.toInt)
    val (x0, x1) = parseRange(xRange)
    val (y0, y1) = parseRange(yRange)
    TargetArea(x0, x1, y0, y1)

extension (t: TargetArea)
  def highestY(vx: Int, vy: Int): Option[Int] =
    @annotation.tailrec
    def loop(x: Int, y: Int, vx: Int, vy: Int, yMax: Int): Option[Int] =
      if t.x0 <= x && x <= t.x1 && t.y0 <= y && y <= t.y1 then Some(yMax)
      else if y < t.y0 then None
      else loop(x + vx, y + vy, vx - vx.sign, vy - 1, yMax max y)
    loop(0, 0, vx, vy, 0)

def solve(s: String): Seq[Int] =
  val targetArea = TargetArea.parse(s)
  for
    vx   <- 1 to targetArea.x1
    vy   <- targetArea.y0 to targetArea.y0.abs
    yMax <- targetArea.highestY(vx, vy)
  yield yMax

def solve1(s: String): BigInt =
  solve(s).max
end solve1

def solve2(s: String): BigInt =
  solve(s).size
end solve2

@main def part1: Unit =
  val ans = solve1(input)
  println(ans)

@main def part2: Unit =
  val ans = solve2(input)
  println(ans)

val input = """target area: x=282..314, y=-80..-45"""
