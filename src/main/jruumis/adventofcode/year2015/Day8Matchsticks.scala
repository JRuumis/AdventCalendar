package jruumis.adventofcode.year2015

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day8Matchsticks extends App {

    val stringsSource: Vector[String] = scala.io.Source.fromFile("./Sources/2015/Day8Matchsticks.txt").getLines().toVector

    val quotePattern: Regex = """(")(.*)""".r
    val singleEscapePattern: Regex = """(\\)(.*)""".r
    val doubleEscapePattern: Regex = """(\\\\)(.*)""".r
    val embeddedQuotePattern: Regex = """(\\")(.*)""".r
    val hexaCharPattern: Regex = """(\\x..)(.*)""".r
    val nextCharPattern: Regex = """(.)(.*)""".r

    @tailrec
    def countStringChars(s: String, accuChars: Int = 0): Int = s match {
        case "" => accuChars-2
        case doubleEscapePattern(_,b) => countStringChars(b, accuChars+1)
        case quotePattern(_,b) => countStringChars(b, accuChars+1)
        case embeddedQuotePattern(_,b) => countStringChars(b, accuChars+1)
        case hexaCharPattern(_,b) => countStringChars(b, accuChars+1)
        case nextCharPattern(_,b) => countStringChars(b, accuChars+1)
    }

    val fullAndCharLength: Vector[(Int, Int)] = stringsSource.map(s => {
        (s.length, countStringChars(s))
    })

    val total: Int = fullAndCharLength.map{case(a,b) => a-b}.sum

    println(s"Number of string code minus total number of chars: ${total}")

    @tailrec
    def encode(s: String, accuChars: Int = 0): Int = s match {
        case "" => accuChars+2
        case quotePattern(_,b) => encode(b, accuChars+2)
        case singleEscapePattern(_,b) => encode(b, accuChars+2)
        case nextCharPattern(_,b) => encode(b, accuChars+1)
    }

    val part2lengths: Vector[(Int, Int)] = stringsSource.map{s => (encode(s), s.length)}
    val total2: Int = part2lengths.map{case(a,b) => a-b}.sum

    println(s"Part 2: ${total2}")
}