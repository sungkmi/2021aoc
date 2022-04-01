package lascala.aoc2021.day11_20.day16

sealed trait BITS
object BITS:
  final case class Literal(version: Int, value: BigInt) extends BITS
  final case class Operator(version: Int, typeId: Int, subpackets: Seq[BITS])
      extends BITS

  def decode(s: String): (BITS, String) =
    val binary = s.map { ch =>
      ("000" + BigInt(ch.toString, 16).toString(2)).takeRight(4)
    }.mkString
    decodeBinary(binary)

  def decodeBinary(binary: String): (BITS, String) =
    extension (binary: String)
      def getIntAndRemainder(splitAt: Int): (Int, String) =
        val (int, remainder) = binary.splitAt(splitAt)
        (BigInt(int, 2).toInt, remainder)
    val (v, binary1) = binary.getIntAndRemainder(3)
    val (t, binary2) = binary1.getIntAndRemainder(3)
    t match
      case 4 =>
        val groups   = binary2.sliding(5, 5).toSeq
        val (v0, r0) = groups.span(_.head == '1')
        val valueStr = (v0 :+ r0.head).map(_.tail).mkString
        (Literal(v, BigInt(valueStr, 2)), r0.tail.mkString)
      case typeId =>
        val (i, binary3) = binary2.getIntAndRemainder(1)
        i match
          case 0 =>
            val (l, others) = binary3.getIntAndRemainder(15)
            val (subs, r)   = others.splitAt(l)
            @annotation.tailrec
            def loop(xs: String, acc: List[BITS]): List[BITS] =
              if xs.isEmpty then acc.reverse
              else
                val (x, r) = decodeBinary(xs)
                loop(r, x :: acc)
            (BITS.Operator(v, typeId, loop(subs, Nil)), r)
          case 1 =>
            val (l, others) = binary3.getIntAndRemainder(11)
            val (acc, remainder) =
              (0 until l).foldLeft((List.empty[BITS], others)) {
                case ((acc, bits), _) =>
                  val (x, r) = decodeBinary(bits)
                  (x :: acc, r)
              }
            (Operator(v, typeId, acc.reverse), remainder)
end BITS

extension (b: BITS)
  def sumOfVersionNumbers: BigInt = b match
    case BITS.Literal(v, _) => BigInt(v)
    case BITS.Operator(v, _, subs) =>
      BigInt(v) + subs.map(_.sumOfVersionNumbers).sum

  def evaluate: BigInt = b match
    case BITS.Literal(_, value) => value
    case BITS.Operator(_, typeId, subs) =>
      val evaluated = subs.map(_.evaluate)
      def check(op: (BigInt, BigInt) => Boolean): BigInt =
        val Seq(first, second) = evaluated
        if op(first, second) then 1 else 0
      typeId match
        case 0 => evaluated.sum
        case 1 => evaluated.product
        case 2 => evaluated.min
        case 3 => evaluated.max
        case 5 => check(_ > _)
        case 6 => check(_ < _)
        case 7 => check(_ == _)
end extension

def solve1(s: String): BigInt =
  BITS.decode(s)._1.sumOfVersionNumbers
end solve1

def solve2(s: String): BigInt =
  BITS.decode(s)._1.evaluate
end solve2

@main def part1: Unit =
  val ans = solve1(input)
  println(ans)

@main def part2: Unit =
  val ans = solve2(input)
  println(ans)

val input =
  """E20D72805F354AE298E2FCC5339218F90FE5F3A388BA60095005C3352CF7FBF27CD4B3DFEFC95354723006C401C8FD1A23280021D1763CC791006E25C198A6C01254BAECDED7A5A99CCD30C01499CFB948F857002BB9FCD68B3296AF23DD6BE4C600A4D3ED006AA200C4128E10FC0010C8A90462442A5006A7EB2429F8C502675D13700BE37CF623EB3449CAE732249279EFDED801E898A47BE8D23FBAC0805527F99849C57A5270C064C3ECF577F4940016A269007D3299D34E004DF298EC71ACE8DA7B77371003A76531F20020E5C4CC01192B3FE80293B7CD23ED55AA76F9A47DAAB6900503367D240522313ACB26B8801B64CDB1FB683A6E50E0049BE4F6588804459984E98F28D80253798DFDAF4FE712D679816401594EAA580232B19F20D92E7F3740D1003880C1B002DA1400B6028BD400F0023A9C00F50035C00C5002CC0096015B0C00B30025400D000C398025E2006BD800FC9197767C4026D78022000874298850C4401884F0E21EC9D256592007A2C013967C967B8C32BCBD558C013E005F27F53EB1CE25447700967EBB2D95BFAE8135A229AE4FFBB7F6BC6009D006A2200FC3387D128001088E91121F4DED58C025952E92549C3792730013ACC0198D709E349002171060DC613006E14C7789E4006C4139B7194609DE63FEEB78004DF299AD086777ECF2F311200FB7802919FACB38BAFCFD659C5D6E5766C40244E8024200EC618E11780010B83B09E1BCFC488C017E0036A184D0A4BB5CDD0127351F56F12530046C01784B3FF9C6DFB964EE793F5A703360055A4F71F12C70000EC67E74ED65DE44AA7338FC275649D7D40041E4DDA794C80265D00525D2E5D3E6F3F26300426B89D40094CCB448C8F0C017C00CC0401E82D1023E0803719E2342D9FB4E5A01300665C6A5502457C8037A93C63F6B4C8B40129DF7AC353EF2401CC6003932919B1CEE3F1089AB763D4B986E1008A7354936413916B9B080"""
