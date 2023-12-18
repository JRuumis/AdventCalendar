package jruumis.adventofcode.year2022

import scala.util.parsing.combinator.JavaTokenParsers

object  Day13DistressSignal extends App {

    // -= Packet =-
    abstract class PacketData

    case class PacketInteger(integer: Int) extends PacketData {
        override def toString: String = integer.toString
    }

    case class PacketList(list: List[PacketData]) extends PacketData {
        override def toString: String = "[" + list.map(_.toString()).mkString(",") + "]"
    }

    case class PacketPair(pairIndex: Int, left: PacketData, right: PacketData) {
        def compare(currentLeft: PacketData = left, currentRight: PacketData = right): Option[Boolean] = {
            println(s"Pair ${pairIndex}: - Compare ${currentLeft} vs ${currentRight}")

            (currentLeft, currentRight) match {

                case (l: PacketList, r: PacketList) if l.list.isEmpty && r.list.isEmpty => {
                    None
                }
                case (l: PacketList, r: PacketList) if l.list.isEmpty => {
                    println(s"Pair ${pairIndex}: - Left side ran out of items, so inputs are in the right order")
                    Some(true)
                }
                case (l: PacketList, r: PacketList) if r.list.isEmpty => {
                    println(s"Pair ${pairIndex}: - Right side ran out of items, so inputs are in the wrong order")
                    Some(false)
                }
                case (l: PacketList, r: PacketList) => compare(l.list.head, r.list.head) match {
                    case Some(c) => Some(c)
                    case _ => compare(PacketList(l.list.tail), PacketList(r.list.tail))
                }
                case (l: PacketInteger, r: PacketList) => {
                    val newLeft = PacketList(List(l))
                    println(s"Pair ${pairIndex}: - Mixed types; convert left to ${newLeft} and retry comparison")
                    compare(newLeft, r)
                }
                case (l: PacketList, r: PacketInteger) => {
                    val newRight = PacketList(List(r))
                    println(s"Pair ${pairIndex}: - Mixed types; convert right to ${newRight} and retry comparison")
                    compare(l, newRight)
                }
                case (l: PacketInteger, r: PacketInteger) if l.integer == r.integer => {
                    None
                }
                case (l: PacketInteger, r: PacketInteger) if l.integer < r.integer => {
                    println(s"Pair ${pairIndex}: - Left side is smaller, so inputs are in the right order")
                    Some(true)
                }
                case (l: PacketInteger, r: PacketInteger) if l.integer > r.integer => {
                    println(s"Pair ${pairIndex}: - Right side is smaller, so inputs are not in the right order")
                    Some(false)
                }
            }
        }
    }


    // -= Parsers =-
    class PacketParser extends JavaTokenParsers {
        def packetIntegerParser: Parser[PacketInteger] = """[0-9]+""".r ^^ { pi => PacketInteger(pi.toInt) }

        def packetListElementParser: Parser[PacketData] = packetIntegerParser | packetListParser ^^ {
            //case (pi: PacketInteger) => pi
            case (pl: PacketList) => pl
        }

        def packetListParser: Parser[PacketList] = "[" ~ repsep(packetListElementParser, ",") ~ "]" ^^ {
            case (_ ~ (elemList: List[PacketData]) ~ _) => PacketList(elemList)
        }
    }

    object DoParse extends PacketParser {
        def parsePacket[T](p: Parser[T], s: String): ParseResult[T] = parseAll(p, s)
    }


    // -= MAIN =-
    val packetInputRaw: String = scala.io.Source.fromFile("./Sources/Day13DistressSignal.txt").mkString
    val packetPairsRaw: Vector[Vector[String]] = packetInputRaw.split("\r\n\r\n").toVector.map(_.split("\r\n").toVector)

    val parsedSignalPairs: Vector[PacketPair] = packetPairsRaw
        .map(pairString => pairString.map(packetString => DoParse.parsePacket(DoParse.packetListParser, packetString)))
        .zipWithIndex
        .map { case (Vector(a, b), i) => PacketPair(i + 1, a.get, b.get) }

    // -= Part 1 =-
    val signalPairComparisons = parsedSignalPairs.map(f => (f.pairIndex, f.compare()))
    val sumOfCorrectlyOrderedSignalPairIdexes: Int = signalPairComparisons.filter { case (_, Some(c)) => c }.map { case (i, _) => i }.sum

    println(s"\nThe sum of correctly ordered signal pair indexes is: ${sumOfCorrectlyOrderedSignalPairIdexes}")


    // -= Part 2 =-
    val dividerOne: PacketList = PacketList(List(PacketList(List(PacketInteger(2)))))
    val dividerTwo: PacketList = PacketList(List(PacketList(List(PacketInteger(6)))))

    val signalsForOrdering: Vector[PacketData] = parsedSignalPairs.flatMap(pair => Vector(pair.left, pair.right)) :+ dividerOne :+ dividerTwo

    val orderedSignals: Vector[(Int, PacketData)] = signalsForOrdering
        .sortWith((p1, p2) => PacketPair(0, p1, p2).compare().get)
        .zipWithIndex.map { case (sigVal, i) => (i + 1, sigVal) }

    val dividerOneIndex: Int = orderedSignals.find { case (_, s) => s == dividerOne }.map { case (i, _) => i }.get
    val dividerTwoIndex: Int = orderedSignals.find { case (_, s) => s == dividerTwo }.map { case (i, _) => i }.get

    println(s"\nThe product of Divider One and Divider Two indexes in the sorted list are: ${dividerOneIndex * dividerTwoIndex}")
}
