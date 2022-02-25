package lascala.aoc2021.day11_20.day12

opaque type Cave = String
extension (c: Cave) def isLarge: Boolean = c.forall(_.isUpper)
object Cave:
  val Start = "start"
  val End   = "end"

opaque type CaveSystem = Map[Cave, Set[Cave]]

extension (cs: CaveSystem)
  def paths(cond: (Cave, List[Cave]) => Boolean): Set[List[Cave]] =
    def dfs(current: Cave, visited: List[Cave]): Set[List[Cave]] =
      val nextVisited = current :: visited
      if current == Cave.End then Set(nextVisited)
      else
        for
          next  <- cs(current) if cond(next, nextVisited)
          paths <- dfs(next, nextVisited)
        yield paths
    dfs(Cave.Start, Nil)
end extension

object CaveSystem:
  def parse(s: String): CaveSystem = s
    .split("\n")
    .flatMap { line =>
      val Array(c0, c1) = line.split("-")
      Array(c0 -> c1, c1 -> c0)
    }
    .groupMapReduce(_._1)((_, v) => Set(v))(_ ++ _)

  def firstCondition(next: Cave, visited: List[Cave]): Boolean =
    next.isLarge || !visited.contains(next)

  def secondCondition(next: Cave, visited: List[Cave]): Boolean =
    val isLarge = next.isLarge
    val isStart = next == Cave.Start
    val isAlreadyVisitedSmallCaveTwice =
      val smalls = visited.filterNot(_.isLarge)
      smalls.size != smalls.toSet.size
    val isAlreadyVisited = visited.contains(next)
    isLarge || !(isStart || (isAlreadyVisitedSmallCaveTwice && isAlreadyVisited))

end CaveSystem

def solve1(s: String): BigInt =
  val caveSystem = CaveSystem.parse(s)
  caveSystem.paths(CaveSystem.firstCondition).size
end solve1

def solve2(s: String): BigInt =
  val caveSystem = CaveSystem.parse(s)
  caveSystem.paths(CaveSystem.secondCondition).size
end solve2

@main def part1: Unit =
  val ans = solve1(input)
  println(ans)

@main def part2: Unit =
  val ans = solve2(input)
  println(ans)

val input = """start-YY
av-rz
rz-VH
fh-av
end-fh
sk-gp
ae-av
YY-gp
end-VH
CF-qz
qz-end
qz-VG
start-gp
VG-sk
rz-YY
VH-sk
rz-gp
VH-av
VH-fh
sk-rz
YY-sk
av-gp
rz-qz
VG-start
sk-fh
VG-av
"""
