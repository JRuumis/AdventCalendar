package jruumis.adventofcode.year2021

object Day2Dive extends App {
    val movesRaw = scala.io.Source.fromFile("./Sources/2021/Day2Dive.txt").getLines().toVector

    case class Coord(y: Int, x: Int) {
        def +(other: Coord): Coord = Coord(this.y + other.y, this.x + other.x)
    }

    // Part One
    val moves = movesRaw.map { _ match {
        case s"forward $i" => Coord(0,i.toInt)
        case s"up $i" => Coord(-i.toInt,0)
        case s"down $i" => Coord(i.toInt,0)
    }}

    val sumMoves: Coord = moves.reduce(_ + _)

    println(s"Position product: ${sumMoves.y * sumMoves.x}")

    // Part Two
    def subMover(movesRaw: Vector[String], currentAim: Int = 0, currentCoord: Coord = Coord(0,0)): Coord = movesRaw match {
        case Vector() => currentCoord
        case s"forward $i" +: rest => subMover(rest, currentAim, currentCoord + Coord(currentAim * i.toInt, i.toInt) )
        case s"up $i" +: rest => subMover(rest, currentAim - i.toInt, currentCoord)
        case s"down $i" +: rest => subMover(rest, currentAim + i.toInt, currentCoord)
    }

    val subCoord: Coord = subMover(movesRaw)

    println(s"Position product: ${subCoord.y * subCoord.x}")
}