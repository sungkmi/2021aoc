package lascala.aoc2021.day11_20.day14

opaque type Polymer = Seq[Char]
object Polymer:
  def apply(s: String): Polymer = s.toSeq

opaque type PairInsertion = Map[(Char, Char), Char]
opaque type PairCount     = Map[(Char, Char), BigInt]

extension [K](s: Seq[(K, BigInt)])
  def mapReduce: Map[K, BigInt] = s.groupMapReduce(_._1)(_._2)(_ + _)

extension (p: Polymer)
  def toPairCount: PairCount = p
    .sliding(2)
    .toSeq
    .groupMapReduce { case Seq(a, b) => (a, b) }(_ => BigInt(1))(_ + _)
  def mkString: String = p.mkString

extension (pc: PairCount)
  def step(pi: PairInsertion): PairCount = pc.toSeq.flatMap {
    case ((a, b), count) =>
      pi.get((a, b)).fold(Seq((a, b) -> count)) { c =>
        Seq((a, c) -> count, (c, b) -> count)
      }
  }.mapReduce

  def elementCountWithLast(last: Char): Map[Char, BigInt] = pc.toSeq
    .map { case ((a, _), count) => (a, count) }
    .appended(last -> BigInt(1))
    .mapReduce

def parse(s: String): (Polymer, PairInsertion) =
  val Array(template, rules) = s.split("\n\n")
  val pairInsertion: PairInsertion = rules
    .split("\n")
    .map { line =>
      val Array(k, v) = line.split(" -> ")
      (k.head, k.last) -> v.head
    }
    .toMap

  (Polymer(template), pairInsertion)

def solve(s: String, numberOfStep: Int): BigInt =
  val (polymer, pairInsertion) = parse(s)
  val last: Char               = polymer.mkString.last
  val p1: PairCount = (0 until numberOfStep)
    .foldLeft(polymer.toPairCount) { (pc, _) => pc.step(pairInsertion) }
  val elementCount = p1.elementCountWithLast(last)
  val counts       = elementCount.values
  counts.max - counts.min

def solve1(s: String): BigInt =
  solve(s, 10)
end solve1

def solve2(s: String): BigInt =
  solve(s, 40)
end solve2

@main def part1: Unit =
  val ans = solve1(input)
  println(ans)

@main def part2: Unit =
  val ans = solve2(input)
  println(ans)

val input = """KOKHCCHNKKFHBKVVHNPN

BN -> C
OS -> K
BK -> C
KO -> V
HF -> K
PS -> B
OK -> C
OC -> B
FH -> K
NV -> F
HO -> H
KK -> H
CV -> P
SC -> C
FK -> N
VV -> F
FN -> F
KP -> O
SB -> O
KF -> B
CH -> K
VF -> K
BH -> H
KV -> F
CO -> N
PK -> N
NH -> P
NN -> C
PP -> H
SH -> N
VO -> O
NC -> F
BC -> B
HC -> H
FS -> C
PN -> F
CK -> K
CN -> V
HS -> S
CB -> N
OF -> B
OV -> K
SK -> S
HP -> C
SN -> P
SP -> B
BP -> C
VP -> C
BS -> K
FV -> F
PH -> P
FF -> P
VK -> F
BV -> S
VB -> S
BF -> O
BB -> H
OB -> B
VS -> P
KB -> P
SF -> N
PF -> S
HH -> P
KN -> K
PC -> B
NB -> O
VC -> P
PV -> H
KH -> O
OP -> O
NF -> K
HN -> P
FC -> H
PO -> B
OH -> C
ON -> N
VN -> B
VH -> F
FO -> B
FP -> B
BO -> H
CC -> P
CS -> K
NO -> V
CF -> N
PB -> H
KS -> P
HK -> S
HB -> K
HV -> O
SV -> H
CP -> S
NP -> N
FB -> B
KC -> V
NS -> P
OO -> V
SO -> O
NK -> K
SS -> H
"""
