object Day3GearRatios_OLD extends App {

    //val inputRaw: Vector[String] = scala.io.Source.fromFile("./Sources/2023/Day3GearRatios_TEST1.txt").getLines().toVector
    val inputRaw: Vector[String] = scala.io.Source.fromFile("./Sources/2023/Day3GearRatios.txt").getLines().toVector

    println(inputRaw.mkString("\n"))


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

        def isNeighbourOf(other: Coord) = allNeighbours contains other
    }

    case class PartNumber(number: Int, numberCoords: Vector[Coord])
    case class Symbol(symbol: Char, coord: Coord)



    def scanLine(
                    line: Vector[Char],
                    curCoord: Coord,
                    curNumber: Vector[Char] = Vector(),
                    curNumberCoords: Vector[Coord] = Vector(),
                    accuNumbers: Vector[PartNumber] = Vector(),
                    accuSymbols: Vector[Symbol] = Vector()
                ): (Vector[PartNumber], Vector[Symbol]) = (line, curNumber) match {

        case (Vector(), Vector())   => (accuNumbers, accuSymbols)
        case (Vector(), _)          => (accuNumbers :+ PartNumber(curNumber.mkString.toInt, curNumberCoords), accuSymbols)

        case (c +: rest, Vector()) if c.isDigit => scanLine(rest, curCoord.moveRight, Vector(c), Vector(curCoord), accuNumbers, accuSymbols)
        case (c +: rest, _) if c.isDigit        => scanLine(rest, curCoord.moveRight, curNumber :+ c, curNumberCoords :+ curCoord, accuNumbers, accuSymbols)

        case (c +: rest, Vector()) if c == '.'  => scanLine(rest, curCoord.moveRight, Vector(), Vector(), accuNumbers, accuSymbols)
        case (c +: rest, _) if c == '.'         => scanLine(rest, curCoord.moveRight, Vector(), Vector(), accuNumbers :+ PartNumber(curNumber.mkString.toInt, curNumberCoords), accuSymbols)

        case (c +: rest, Vector()) if !("0123456789." contains c)   => scanLine(rest, curCoord.moveRight, Vector(), Vector(), accuNumbers, accuSymbols :+ Symbol(c, curCoord))
        case (c +: rest, _) if !("0123456789." contains c)          => scanLine(rest, curCoord.moveRight, Vector(), Vector(), accuNumbers :+ PartNumber(curNumber.mkString.toInt, curNumberCoords), accuSymbols :+ Symbol(c, curCoord))
    }


    val lineNumbersSymbols = inputRaw.zipWithIndex.map{case(s,i) => scanLine( s.toVector, Coord(i,0) )}

    val (numbers, symbols) = lineNumbersSymbols.unzip match {case(a,b) => (a.flatten, b.flatten)}

    println(lineNumbersSymbols.mkString("\n"))

    println(numbers)
    println
    println(symbols)
    println

    val symbolCoords = symbols.map(_.coord)


    val numbersNextToSymbols = numbers.filter( num => {
        num.numberCoords.flatMap(_.allNeighbours).map(symbolCoords contains _).reduce(_ || _)
    })

    println("numbers next to symbols")
    println(numbersNextToSymbols)

    val xxx = numbersNextToSymbols.map(_.number)
    println(xxx)
    println(xxx.sum)



    // Part Two
    val gearCandidates: Vector[Symbol] = symbols.filter(s => s.symbol == '*')

    println(s"gear candidates: $gearCandidates")

    val gears: Vector[(Symbol, Vector[PartNumber])] = gearCandidates.map{ gc => {

        val gearNeigbourCoords: Vector[Coord] = gc.coord.allNeighbours

        val neighbourParts: Vector[PartNumber] = numbersNextToSymbols.filter(num => num.numberCoords.map(gearNeigbourCoords contains _).reduce(_ || _))

        (gc, neighbourParts)
    }}.filter{case(a,b) => b.size==2}

    println
    println("gears")
    println(gears)

    val xxxxx = gears.map{case(g,n) => n.map(_.number).reduce(_ * _)}.sum

    println(xxxxx)



}
