package lascala.aoc2021.day11_20.day18

sealed trait SnailfishNumber
object SnailfishNumber:
  final case class RegularNumber(value: Int) extends SnailfishNumber
  final case class Pair(left: SnailfishNumber, right: SnailfishNumber)
      extends SnailfishNumber

  def apply(s: String): SnailfishNumber = decode(s.toList)._1

  def decode(input: List[Char]): (SnailfishNumber, List[Char]) =
    input.headOption match
      case Some(d) if d.isDigit => (RegularNumber(d - '0'), input.tail)
      case Some('[') =>
        val (left, rest) = decode(input.tail)
        assert(rest.headOption == Some(','), "Expected ','")
        val (right, rest2) = decode(rest.tail)
        assert(rest2.headOption == Some(']'), "Expected ']'")

        (Pair(left, right), rest2.tail)
      case _ => throw Exception("Invalid input")

import SnailfishNumber.*

extension (sn: SnailfishNumber)

  def show: String = sn match
    case RegularNumber(n)  => n.toString
    case Pair(left, right) => s"[${left.show},${right.show}]"

  def +(that: SnailfishNumber): SnailfishNumber =
    Pair(sn, that).reduce

  def addRight(n: Int): SnailfishNumber = sn match
    case RegularNumber(v)  => RegularNumber(v + n)
    case Pair(left, right) => Pair(left, right.addRight(n))

  def addLeft(n: Int): SnailfishNumber = sn match
    case RegularNumber(v)  => RegularNumber(v + n)
    case Pair(left, right) => Pair(left.addLeft(n), right)

  def reduce: SnailfishNumber =
    findExplode(0).map(_._1.reduce) orElse findSplit.map(_.reduce) getOrElse sn

  def findExplode(
      depth: Int,
  ): Option[(SnailfishNumber, Option[Int], Option[Int])] = sn match
    case Pair(RegularNumber(l), RegularNumber(r)) if depth >= 4 =>
      Some((RegularNumber(0), Some(l), Some(r)))
    case Pair(left, right) =>
      def examineLeft = left.findExplode(depth + 1).map {
        case (leftNext, leftExplode, rightExplode) =>
          val rightNext = rightExplode.fold(right)(right.addLeft)
          (Pair(leftNext, rightNext), leftExplode, None)
      }
      def examineRight = right.findExplode(depth + 1).map {
        case (rightNext, leftExplode, rightExplode) =>
          val leftNext = leftExplode.fold(left)(left.addRight)
          (Pair(leftNext, rightNext), None, rightExplode)
      }
      examineLeft orElse examineRight
    case _ => None

  def findSplit: Option[SnailfishNumber] = sn match
    case RegularNumber(n) =>
      if n < 10 then None
      else Some(Pair(RegularNumber(n / 2), RegularNumber((n + 1) / 2)))
    case Pair(left, right) =>
      left.findSplit.map(Pair(_, right)) orElse right.findSplit.map(
        Pair(left, _),
      )

  def magnitude: BigInt = sn match
    case RegularNumber(n)  => BigInt(n)
    case Pair(left, right) => 3 * left.magnitude + 2 * right.magnitude

end extension

extension (s: String)
  def toNumbers: Seq[SnailfishNumber] = s.split("\n").map(SnailfishNumber(_))

def sum(n: Seq[SnailfishNumber]): SnailfishNumber = n.reduceLeft(_ + _)

def solve1(s: String): BigInt =
  sum(s.toNumbers).magnitude
end solve1

def solve2(s: String): BigInt =
  val numbers = s.toNumbers
  val pairMagnitudes = for
    x <- numbers
    y <- numbers if x != y
  yield (x + y).magnitude

  pairMagnitudes.max
end solve2

@main def part1: Unit =
  val ans = solve1(input)
  println(ans)

@main def part2: Unit =
  val ans = solve2(input)
  println(ans)

val input = """[[6,[[9,4],[5,5]]],[[[0,7],[7,8]],[7,0]]]
[[[[2,1],[8,6]],[2,[4,0]]],[9,[4,[0,6]]]]
[[[[4,2],[7,7]],4],[3,5]]
[8,[3,[[2,3],5]]]
[[[[0,0],[4,7]],[[5,5],[8,5]]],[8,0]]
[[[[5,2],[5,7]],[1,[5,3]]],[[4,[8,4]],2]]
[[5,[[2,8],[9,3]]],[[7,[5,2]],[[9,0],[5,2]]]]
[[9,[[4,3],1]],[[[9,0],[5,8]],[[2,6],1]]]
[[0,6],[6,[[6,4],[7,0]]]]
[[[9,[4,2]],[[6,0],[8,9]]],[[0,4],[3,[6,8]]]]
[[[[3,2],0],[[9,6],[3,1]]],[[[3,6],[7,6]],[2,[6,4]]]]
[5,[[[1,6],[7,8]],[[6,1],[3,0]]]]
[2,[[6,[7,6]],[[8,6],3]]]
[[[[0,9],1],[2,3]],[[[7,9],1],7]]
[[[[1,8],3],[[8,8],[0,8]]],[[2,1],[8,0]]]
[[2,9],[[5,1],[[9,3],[4,0]]]]
[9,[8,4]]
[[[3,3],[[6,2],8]],5]
[[[9,[4,8]],[[1,3],[6,7]]],[9,[[4,4],2]]]
[[[[1,3],6],[[5,6],[1,9]]],[9,[[0,2],9]]]
[7,[[[0,6],[1,2]],4]]
[[[[5,0],[8,7]],[[7,3],0]],[[6,7],[0,1]]]
[[[[5,4],7],[[8,2],1]],[[[7,0],[6,9]],0]]
[[[3,[5,6]],[[9,5],4]],[[[9,4],[8,1]],[5,[7,4]]]]
[[[3,[7,5]],[[8,1],8]],[[[6,3],[9,2]],[[5,7],7]]]
[8,[[2,0],[[2,6],8]]]
[[[[5,8],9],1],[9,6]]
[[[9,9],[8,8]],[[[3,5],[8,0]],[[4,6],[3,2]]]]
[[5,[[5,1],6]],[[5,8],9]]
[[7,[[1,6],6]],[[[8,6],7],[6,6]]]
[[0,[[9,5],0]],[4,[[7,9],[4,9]]]]
[[[[4,3],[3,5]],[[1,9],[7,6]]],[3,[[6,4],[6,0]]]]
[[[2,6],6],[6,3]]
[[[[1,5],[3,7]],0],[3,7]]
[4,[[[5,5],4],[[5,5],[9,3]]]]
[[3,[8,6]],[8,[7,7]]]
[8,[9,5]]
[[[6,3],[2,[3,6]]],[[[6,0],[0,2]],[[8,7],5]]]
[[[8,[1,2]],2],7]
[[[[8,4],[2,7]],[[3,9],7]],[[4,[8,8]],[[7,4],9]]]
[[[8,[2,5]],[3,[1,2]]],[[4,[5,0]],3]]
[[8,[0,3]],[[5,1],[1,1]]]
[[[8,[3,6]],6],[[7,[1,5]],[[4,8],9]]]
[[[5,0],[0,3]],[[2,[7,8]],[1,[4,8]]]]
[9,[4,[9,4]]]
[[[9,[0,4]],2],3]
[[9,[7,[8,9]]],3]
[[[8,6],[[3,5],[9,2]]],[[3,[9,7]],5]]
[[6,[[7,4],2]],[2,[7,[6,0]]]]
[1,[[[2,2],6],8]]
[[[6,[1,8]],[[9,3],[1,8]]],[[[8,2],[9,3]],[[8,2],[9,9]]]]
[[[[2,9],[1,7]],[[4,0],8]],[[8,9],[6,3]]]
[[[[2,4],[6,1]],[[5,4],[2,8]]],[8,[1,[2,4]]]]
[[[4,6],[1,6]],[3,[1,1]]]
[[[[8,3],8],8],[1,[[4,2],3]]]
[[[9,[8,7]],[5,9]],[8,[[5,6],[4,5]]]]
[[[[4,1],2],[[7,8],4]],[0,6]]
[[[9,7],[[8,6],[6,9]]],[[8,[8,4]],[[9,0],2]]]
[[[8,5],[1,9]],[[[2,4],5],6]]
[[[9,[9,3]],[9,[2,3]]],[7,7]]
[[[8,[7,4]],[2,6]],[[[4,5],[9,9]],[0,[5,2]]]]
[7,[2,2]]
[[[[1,8],[5,2]],3],[0,[2,[4,5]]]]
[[5,[[4,8],[5,5]]],[4,[[3,4],[6,0]]]]
[[3,1],[4,[3,[8,2]]]]
[[3,7],[3,[[6,1],[0,2]]]]
[[4,[6,2]],[[3,9],8]]
[[[[2,9],3],[[5,6],4]],[8,2]]
[[4,[[7,9],[4,9]]],[[4,3],[7,[0,7]]]]
[[[3,[8,9]],[[3,4],[9,5]]],3]
[0,[[[3,0],[8,7]],[[0,9],[9,1]]]]
[[[5,[9,9]],2],[4,8]]
[[[[4,4],4],5],[3,4]]
[[[3,[2,2]],7],[[3,2],0]]
[[[[0,5],[5,2]],2],[2,[[1,2],2]]]
[[[4,6],6],[[0,1],6]]
[2,[[[3,9],7],[[9,8],8]]]
[[7,9],[7,[[3,0],9]]]
[[[1,[6,2]],[0,8]],[[[7,2],4],9]]
[[[[4,7],[1,5]],[5,9]],[[2,[0,4]],[7,[7,0]]]]
[[1,[[2,0],[0,4]]],[[[4,6],9],[[6,8],[0,1]]]]
[[[[6,0],7],[7,[9,6]]],[[7,[4,9]],[9,4]]]
[[[5,[4,6]],[[1,9],[5,8]]],[[[3,6],[2,6]],[[7,3],7]]]
[[[6,0],[6,6]],[2,8]]
[[[4,[7,2]],[[5,6],[2,4]]],[[[6,8],5],[4,6]]]
[[[[9,0],9],[4,0]],[[[9,1],8],[6,4]]]
[[6,3],[1,[[5,0],[9,9]]]]
[[[2,7],[5,6]],[[6,[1,4]],[9,9]]]
[[[[0,5],3],[8,7]],[[[9,9],[6,2]],[0,7]]]
[[[5,6],[1,7]],[[[0,4],9],9]]
[[[7,3],3],[6,[0,[8,9]]]]
[[[0,6],[[8,5],[4,6]]],[[[2,7],[4,2]],[[8,7],[0,5]]]]
[[[8,[7,3]],1],8]
[[8,[8,[8,2]]],[[5,4],[1,[2,6]]]]
[[[[1,1],[8,6]],5],9]
[[[[2,4],[5,7]],[[5,8],[3,1]]],7]
[[4,[[0,1],9]],[[3,8],[4,2]]]
[3,2]
[[3,4],[8,[[6,5],[6,6]]]]
[[[[7,0],[3,8]],[[3,3],[2,6]]],[[8,0],9]]
"""
