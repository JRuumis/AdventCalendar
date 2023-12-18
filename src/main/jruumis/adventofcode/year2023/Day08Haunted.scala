package jruumis.adventofcode.year2023

import scala.annotation.tailrec

object Day08Haunted extends App {
    val mapRaw: Vector[String] = scala.io.Source.fromFile("./Sources/2023/Day8Haunted.txt").getLines().toVector

    val instructions: Vector[Char] = mapRaw.head.toVector
    val instructionsLength: Int = instructions.size

    val directionsMap: Map[String, (String, String)] = mapRaw.tail.tail.map{_ match {
        case s"$from = ($toLeft, $toRight)" => Some((from, (toLeft, toRight)))
        case _ => {
            println("Match error!")
            None
        }
    }}.filter(_.isDefined).map(_.get).toMap

    @tailrec
    def walker(curPosition: String, finisher: String => Boolean, curPathStep: Int = 0): Int = {
        if (finisher(curPosition)) curPathStep
        else {
            val curInstruction: Char = instructions(curPathStep % instructionsLength)
            val (nextPositionLeft, nextPositionRight) = directionsMap(curPosition)
            val nextPosition: String =  if(curInstruction == 'L') nextPositionLeft else nextPositionRight

            walker(nextPosition, finisher, curPathStep+1)
        }
    }

    // Part One
    val stepsToReachZZZ: Int = walker("AAA", _ == "ZZZ")
    println(s"Steps to reach ZZZ: $stepsToReachZZZ")

    // Part Two
    val startPositions: Vector[String] = directionsMap.keys.toVector.filter(_.last == 'A')

    @tailrec
    def gcd(a: BigInt, b: BigInt): BigInt = if (b == 0) a else gcd(b, a % b)
    def lcm(a: BigInt, b: BigInt): BigInt = if (a == 0 || b == 0) 0 else (a * b).abs / gcd(a, b)
    def lcm(values: Vector[BigInt]): BigInt = values.foldLeft(BigInt(1))((acc, value) => lcm(acc, value))

    val stepsToZFromAllAs: Vector[BigInt] = startPositions.map(s => walker(s, _.last == 'Z') ).map(i => BigInt(i))
    val stepsForAllAsToReachZ: BigInt = lcm(stepsToZFromAllAs)
    println(s"Steps for all As to reach Z: $stepsForAllAsToReachZ")
}