package jruumis.adventofcode.year2023

import scala.annotation.tailrec

object Day03GearRatios extends App {

    val inputRaw: Vector[String] = scala.io.Source.fromFile("./Sources/2023/Day3GearRatios.txt").getLines().toVector

    case class Coord(y: Int, x: Int) {
        def moveRight: Coord = Coord(y,x+1)

        lazy val allNeighbours: Vector[Coord] = Vector(
            Coord(y - 1, x - 1),
            Coord(y - 1, x),
            Coord(y - 1, x + 1),

            Coord(y, x - 1),
            Coord(y, x + 1),

            Coord(y + 1, x - 1),
            Coord(y + 1, x),
            Coord(y + 1, x + 1)
        )
    }

    case class EnginePart(partNumber: Int, partNumberCoords: Vector[Coord]) {
        lazy val neighbourCoords: Set[Coord] = partNumberCoords.flatMap(_.allNeighbours).toSet
        def isNeighbourOf(symbol: EngineSymbol) = neighbourCoords contains symbol.coord
    }

    case class EngineSymbol(symbol: Char, coord: Coord)

    @tailrec
    def scanLine(
                    line: Vector[Char],
                    curCoord: Coord,
                    curNumber: Vector[Char] = Vector(),
                    curNumberCoords: Vector[Coord] = Vector(),
                    accuNumbers: Vector[EnginePart] = Vector(),
                    accuSymbols: Vector[EngineSymbol] = Vector()
                ): Option[(Vector[EnginePart], Vector[EngineSymbol])] = (line, curNumber) match {

        // line is scanned
        case (Vector(), Vector())   => Some((accuNumbers, accuSymbols))
        case (Vector(), _)          => Some((accuNumbers :+ EnginePart(curNumber.mkString.toInt, curNumberCoords), accuSymbols))

        // current is digit
        case (c +: rest, Vector()) if c.isDigit => scanLine(rest, curCoord.moveRight, Vector(c), Vector(curCoord), accuNumbers, accuSymbols)
        case (c +: rest, _) if c.isDigit        => scanLine(rest, curCoord.moveRight, curNumber :+ c, curNumberCoords :+ curCoord, accuNumbers, accuSymbols)

        // current is .
        case (c +: rest, Vector()) if c == '.'  => scanLine(rest, curCoord.moveRight, Vector(), Vector(), accuNumbers, accuSymbols)
        case (c +: rest, _) if c == '.'         => scanLine(rest, curCoord.moveRight, Vector(), Vector(), accuNumbers :+ EnginePart(curNumber.mkString.toInt, curNumberCoords), accuSymbols)

        // current is symbol
        case (c +: rest, Vector()) if !("0123456789." contains c)   => scanLine(rest, curCoord.moveRight, Vector(), Vector(), accuNumbers, accuSymbols :+ EngineSymbol(c, curCoord))
        case (c +: rest, _) if !("0123456789." contains c)          => scanLine(rest, curCoord.moveRight, Vector(), Vector(), accuNumbers :+ EnginePart(curNumber.mkString.toInt, curNumberCoords), accuSymbols :+ EngineSymbol(c, curCoord))

        case _ => {
            println("Error: missing scanLine case!")
            None
        }
    }

    val (allParts, allSymbols) = inputRaw
        .zipWithIndex
        .map{case(line,ind) => scanLine( line.toVector, Coord(ind,0) )}.filter(_.isDefined).map(_.get)
        .unzip match {case(a,b) => (a.flatten, b.flatten)}

    val partsNextToSymbols: Vector[EnginePart] = allParts.filter(part => allSymbols.map(symb => part isNeighbourOf symb).reduce(_ || _))

    // Part One
    val partNumbersNextToSymbols = partsNextToSymbols.map(_.partNumber).sum
    println(s"The sum of all Part Numbers is $partNumbersNextToSymbols")

    // Part Two
    val gearCandidates: Vector[EngineSymbol] = allSymbols.filter(s => s.symbol == '*')

    val gearsWithNeigbours: Vector[(EngineSymbol, Vector[EnginePart])] = gearCandidates
        .map{ gc => (gc, partsNextToSymbols.filter(_ isNeighbourOf gc)) }
        .filter{case(_,b) => b.size==2}

    val gearRatioSum: Int = gearsWithNeigbours.map{ case(_,neighbourPart) => neighbourPart.map(_.partNumber).reduce(_ * _) }.sum
    println(s"The sum of all Gear Ratios is $gearRatioSum")
}