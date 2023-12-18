package jruumis.adventofcode.year2015

object Day1Lisp extends App {
    val elevatorRaw: Vector[Char] = scala.io.Source.fromFile("./Sources/2015/Day1Lisp.txt").toVector
    val floors: Vector[Int] = elevatorRaw.map(c => if(c == '(') 1 else -1)

    println(s"Elevator stops at floor ${floors.sum}")

    def firstBasement(index: Int, floors: Vector[Int], accuFloor: Int = 0): Int = floors match {
        case _ if accuFloor < 0 => index
        case Vector() => 0
        case f +: rest => firstBasement(index+1, rest, accuFloor+f)
    }

    val firstBasementFloor: Int = firstBasement(0, floors)
    println(s"First press into basement: ${firstBasementFloor}")
}
