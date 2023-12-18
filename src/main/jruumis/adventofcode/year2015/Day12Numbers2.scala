package jruumis.adventofcode.year2015

import scala.util.parsing.combinator.JavaTokenParsers

object Day12Numbers2 extends App {

    val numbersRaw: String = scala.io.Source.fromFile("./Sources/2015/Day12Numbers.txt").mkString

    // Json
    abstract sealed class JsonElement {
        val sumAll: Int
        val sumNoRed: Int
    }
    case class Num(i: Int) extends JsonElement {
        override val sumAll: Int = i
        override val sumNoRed: Int = i
    }
    case class Str(s: String) extends JsonElement {
        override val sumAll: Int = 0
        override val sumNoRed: Int = 0
    }
    case class Arr(l: List[JsonElement]) extends JsonElement {
        override val sumAll: Int = l.map(_.sumAll).sum
        override val sumNoRed: Int = l.map(_.sumNoRed).sum
    }
    case class Obj(l: List[JsonElement]) extends JsonElement {
        override val sumAll: Int = l.map(_.sumAll).sum
        override val sumNoRed: Int = {
            val hasRed = l.map(_ match {
                case Str("red")  => true
                case _ => false
            }).reduce(_ || _)

            if(hasRed) 0 else l.map(_.sumNoRed).sum
        }
    }

    // Parser
    class JsonParser extends JavaTokenParsers {
        def numberParser: Parser[Num] = """-?[0-9]+""".r ^^ { i => Num(i.toInt) }
        def stringParser: Parser[Str] = "\"" ~ "[a-z]+".r ~ "\"" ^^ { case _ ~ s ~ _ => Str(s) }
        def arrayParser: Parser[Arr]  = "[" ~ repsep(elementParser, ",") ~ "]" ^^ { case _ ~ (elemList: List[JsonElement]) ~ _ => Arr(elemList) }
        def objectElement: Parser[JsonElement] = stringParser ~ ":" ~ elementParser ^^ { case _ ~ _ ~ (a: JsonElement) => a }
        def objectParser: Parser[Obj] = "{" ~ repsep(objectElement, ",") ~ "}" ^^ { case _ ~ (elemList: List[JsonElement]) ~ _ => Obj(elemList) }
        def elementParser: Parser[JsonElement] = numberParser | stringParser | arrayParser | objectParser
    }

    object DoParse extends JsonParser {
        def parseInput[T](p: Parser[T], s: String): ParseResult[T] = parseAll(p, s)
    }

    val parsedJson: JsonElement = DoParse.parseInput(DoParse.elementParser, numbersRaw).get

    println(s"Part 1: sum of all numbers: ${parsedJson.sumAll}")
    println(s"Part 2: sum of numbers excluding red: ${parsedJson.sumNoRed}")
}