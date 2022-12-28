object Day23Diffusion extends App {
    val elfPositionsRaw = scala.io.Source.fromFile("./Sources/Day23Diffusion.txt").getLines().toVector

    //println(elfPositionsRaw.mkString("\n"))


    case class Coord(y: Int, x: Int) {
        def +(other: Coord) = Coord(y + other.y, x + other.x)
    }

    val elfPositionsWithIndex = elfPositionsRaw.map(s => s.toVector.zipWithIndex).zipWithIndex
    //println(elfPositionsWithIndex.mkString("\n"))

    val initElfCoords: Set[Coord] = elfPositionsWithIndex.flatMap { case (row, y) => row.filter { case (c, _) => c == '#' }.map { case (_, x) => Coord(y, x) } }.toSet
    //println(initElfCoords)

    case class Move(check: Vector[Coord], move: Coord)

    val roundView: Vector[Coord] = Vector(
        Coord(-1, -1), Coord(-1, 0), Coord(-1, 1),
        Coord(0, -1), Coord(0, 1),
        Coord(1, -1), Coord(1, 0), Coord(1, 1),
    )

    val moves: Vector[Move] = Vector(
        Move(check = Vector(Coord(-1, -1), Coord(-1, 0), Coord(-1, 1)), move = Coord(-1, 0)), // N
        Move(check = Vector(Coord(1, -1), Coord(1, 0), Coord(1, 1)), move = Coord(1, 0)), // S
        Move(check = Vector(Coord(-1, -1), Coord(0, -1), Coord(1, -1)), move = Coord(0, -1)), // W
        Move(check = Vector(Coord(-1, 1), Coord(0, 1), Coord(1, 1)), move = Coord(0, 1)) // E
    )

    def getFirstValidMove(elfCoord: Coord, moves: Vector[Move], elfCoords: Set[Coord]): Option[Coord] = {

        val doesNotHaveNeighbours: Boolean = roundView.map(rw => !(elfCoords contains (elfCoord + rw))).reduce(_ && _)

        if (doesNotHaveNeighbours) None
        else {
            moves.map(m => {
                val isDirectionFree: Boolean = m.check.map(checkCoord => !(elfCoords contains (elfCoord + checkCoord))).reduce(_ && _)
                val moveToCoord: Coord = elfCoord + m.move
                (isDirectionFree, moveToCoord)
            }).find { case (valid, _) => valid } match {
                case Some((_, coord)) => Some(coord)
                case None => None
            }
        }

    }

    val maxTime: Int = 10

    def getRectangle(elfCoords: Set[Coord]): Vector[Vector[Char]] = {
        val minX = elfCoords.map(_.x).min
        val maxX = elfCoords.map(_.x).max
        val minY = elfCoords.map(_.y).min
        val maxY = elfCoords.map(_.y).max

        (0 to maxY - minY).toVector.map(y => (0 to maxX - minX).toVector.map(x => {
            if (elfCoords contains Coord(y + minY, x + minX)) '#' else '.'
        }))
    }

    def iterate(prevElfCoords: Set[Coord], elfCoords: Set[Coord], currentMoves: Vector[Move], currentTime: Int): Set[Coord] = (currentTime, currentMoves) match {
        //case (`maxTime`, _) => elfCoords
        case (_, _) if prevElfCoords == elfCoords && currentTime > 0 => elfCoords
        case (_, m1 +: rest) => {

            val proposedMoves: Vector[(Coord, Option[Coord])] = elfCoords.toVector.map(coord => (coord, getFirstValidMove(coord, currentMoves, elfCoords))) //////
            //println(s"proposed moves: ${proposedMoves}")

            val dupes: Set[Coord] = proposedMoves.map { case (a, b) => b }.filter(_.isDefined).map(_.get).groupBy(i => i).view.mapValues(i => i.size).filter { case (a, c) => c > 1 }.map { case (a, c) => a }.toSet
            //println(s"dupes: ${dupes}")

            val approvedMoves: Map[Coord, Coord] = proposedMoves.map { case (coord, proposedMove) => (coord, proposedMove) match {
                case (c, None) => (c, c)
                case (c, Some(m)) if dupes contains m => (c, c)
                case (c, Some(m)) => (c, m)
            }
            }.toMap

            //println(s"approved moves: ${approvedMoves}")

            val newElfCoords = elfCoords.map(approvedMoves(_))
            val newMoves = rest :+ m1

            println(currentTime + 1)
            //println(s"rectangle: \n${getRectangle(newElfCoords).map(_.mkString).mkString("\n")}")


            iterate(elfCoords, newElfCoords, newMoves, currentTime + 1)
        }
    }

    val resultingCoords: Set[Coord] = iterate(initElfCoords, initElfCoords, moves, 0)

    val resultingRectangle: Vector[Vector[Char]] = getRectangle(resultingCoords)

    println()
    println(resultingRectangle.map(_.mkString).mkString("\n"))

    val resultingDots = resultingRectangle.flatMap(r => r.map(i => if (i == '.') 1 else 0)).sum

    println(resultingDots)


    //println(s"after move: ${resultingCoords}")

}
