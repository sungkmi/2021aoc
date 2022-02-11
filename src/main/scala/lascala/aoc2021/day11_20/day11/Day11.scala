package lascala.aoc2021.day11_20.day11

opaque type Coord       = (Int, Int)
opaque type EnergyLevel = Map[Coord, Int]

extension (c: Coord)
  def adjacent: Seq[Coord] =
    val (x, y) = c
    for
      dx <- -1 to 1
      dy <- -1 to 1 if dx != 0 || dy != 0
    yield (x + dx, y + dy)

extension (e: EnergyLevel)
  def step: (EnergyLevel, Int) =
    @annotation.tailrec
    def loop(e: EnergyLevel, flashSet: Set[Coord]): (EnergyLevel, Set[Coord]) =
      val (unFlash, flashes) = e.partition(_._2 <= 9)
      if flashes.isEmpty then (unFlash, flashSet)
      else
        val unFlash1 = flashes.foldLeft(unFlash) { case (unFlash, (c, _)) =>
          c.adjacent.filter(unFlash.contains).foldLeft(unFlash) {
            case (unFlash, c) =>
              unFlash + (c -> (unFlash(c) + 1))
          }
        }
        loop(unFlash1, flashes.keySet ++ flashSet)
    val (e1, fSet) = loop(e.view.mapValues(_ + 1).toMap, Set.empty)
    (e1 ++ fSet.map { _ -> 0 }, fSet.size)
end extension

object EnergyLevel:
  def parse(s: String): EnergyLevel =
    val parsed = for
      (line, y) <- s.split("\n").zipWithIndex
      (e, x)    <- line.zipWithIndex
    yield (x, y) -> e.toString.toInt
    parsed.toMap
end EnergyLevel

def solve1(s: String): BigInt =
  val e = EnergyLevel.parse(s)
  (1 to 100)
    .foldLeft((e, 0)) { case ((e, acc), _) =>
      val (e1, count) = e.step
      (e1, acc + count)
    }
    ._2
end solve1

def solve2(s: String): BigInt =
  @annotation.tailrec
  def loop(e: EnergyLevel, i: Int): Int =
    val (e1, count) = e.step
    if count == e1.size then i + 1 else loop(e1, i + 1)
  val e = EnergyLevel.parse(s)
  loop(e, 0)
end solve2

@main def part1: Unit =
  val ans = solve1(input)
  println(ans)

@main def part2: Unit =
  val ans = solve2(input)
  println(ans)

val input = """6636827465
6774248431
4227386366
7447452613
6223122545
2814388766
6615551144
4836235836
5334783256
4128344843
"""
