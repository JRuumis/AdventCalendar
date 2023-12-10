import scala.annotation.tailrec

object Day10PipeMaze extends App {

    // Maze
    val mazeGrid: Vector[Vector[Char]] = scala.io.Source.fromFile("./Sources/2023/Day10PipeMaze.txt").getLines().toVector.map(_.toVector)

    val sizeX: Int = mazeGrid.head.size
    val sizeY: Int = mazeGrid.size

    def getMazeChar(coord: Coord): Option[Char] =
        if (coord.x < 0 || coord.y < 0 || coord.x >= sizeX || coord.y >= sizeY) None
        else Some(mazeGrid(coord.y)(coord.x))


    // Coord
    case class Coord(x: Int, y: Int) {
        def +(other: Coord): Coord = Coord(this.x + other.x, this.y + other.y)
        def -(other: Coord): Coord = Coord(this.x - other.x, this.y - other.y)
        lazy val outOfBounds: Boolean = x < 0 || x >= sizeX || y < 0 || y >= sizeY
    }
    object Coord {
        val upCoord = Coord(0, -1)
        val downCoord = Coord(0, 1)
        val leftCoord = Coord(-1, 0)
        val rightCoord = Coord(1, 0)

        val upLeftCoord = Coord(-1, -1)
        val upRightCoord = Coord(1, -1)
        val downLeftCoord = Coord(-1, 1)
        val downRightCoord = Coord(1, 1)

        val allDirections: Set[Coord] = Set(
            upCoord, downCoord, leftCoord, rightCoord,
            upLeftCoord, upRightCoord, downLeftCoord, downRightCoord
        )

        val moves: Map[Char, Set[Coord]] = Map(
            'S' -> Set(upCoord, downCoord, leftCoord, rightCoord),
            'F' -> Set(downCoord, rightCoord),
            '7' -> Set(downCoord, leftCoord),
            'J' -> Set(upCoord, leftCoord),
            'L' -> Set(upCoord, rightCoord),
            '-' -> Set(leftCoord, rightCoord),
            '|' -> Set(upCoord, downCoord),
            '.' -> Set()
        )
    }

    // CornerCoord
    case class CornerCoord(x: Double, y: Double) {
        def forCoord(coord: Coord): CornerCoord = CornerCoord(this.x + coord.x, this.y + coord.y)

        lazy val surroundingCoords: Set[Coord] = CornerCoord.allCorners.map(c => Coord( (this.x + c.x).toInt, (this.y + c.y).toInt ) )
    }
    object CornerCoord {
        val upLeftCorner = CornerCoord(-0.5, -0.5)
        val upRightCorner = CornerCoord(0.5, -0.5)
        val downLeftCorner = CornerCoord(-0.5, 0.5)
        val downRightCorner = CornerCoord(0.5, 0.5)

        val allCorners: Set[CornerCoord] = Set(upLeftCorner, upRightCorner, downLeftCorner, downRightCorner)

        val twoSides: Map[Char, (Set[CornerCoord], Set[CornerCoord])] = Map(
            'F' -> (Set(downLeftCorner, upLeftCorner, upRightCorner), Set(downRightCorner)),
            '7' -> (Set(upLeftCorner, upRightCorner, downRightCorner), Set(downLeftCorner)),
            'J' -> (Set(upRightCorner, downRightCorner, downLeftCorner), Set(upLeftCorner)),
            'L' -> (Set(upLeftCorner, downLeftCorner, downRightCorner), Set(upRightCorner)),
            '-' -> (Set(upLeftCorner, upRightCorner), Set(downLeftCorner, downRightCorner)),
            '|' -> (Set(upLeftCorner, downLeftCorner), Set(upRightCorner, downRightCorner))
        )
    }

    // Cell
    case class Cell(mazeChar: Char, coord: Coord, step: Int) {
        lazy val neighbourCoords: Set[Coord] = {
            Coord.moves(mazeChar).map(moveCoord => coord + moveCoord)
        }

        def isConnectedTo(other: Cell): Boolean = {
            val thisConnectedToOther: Boolean = (Coord.moves(this.mazeChar).map(_ + this.coord).contains(other.coord))
            val otherConnectedToThis: Boolean = (Coord.moves(other.mazeChar).map(_ + other.coord) contains this.coord)

            thisConnectedToOther && otherConnectedToThis
        }
    }

    @tailrec
    def mazeWalker(frontCells: Set[Cell], visitedCells: Set[Cell] = Set(), visitedCoords: Set[Coord] = Set(), currentDepth: Int = 0): Set[Cell] = {
        if (frontCells.isEmpty) visitedCells
        else {
            val nextFrontCells: Set[Cell] = frontCells.flatMap(frontCell =>
                frontCell.neighbourCoords
                    .map(coord => (coord, getMazeChar(coord)))
                    .filter { case (_, char) => char.isDefined } // valid coordinates - not outside the maze
                    .filter { case (coord, _) => !visitedCoords.contains(coord) } // exclude already visited coordinates
                    .map { case (coord, Some(char)) => Cell(char, coord, currentDepth + 1) }
                    .filter(_ isConnectedTo frontCell) // exclude neighbours that are not properly connected
            )
            mazeWalker(nextFrontCells, visitedCells ++ frontCells, visitedCoords ++ frontCells.map(_.coord), currentDepth+1)
        }
    }

    @tailrec
    def sideWalker(cellsInPath: Vector[Cell], accuSideA: Set[CornerCoord] = Set(), accuSideB: Set[CornerCoord] = Set()): (Set[CornerCoord], Set[CornerCoord]) = cellsInPath match {
        case Vector() => (accuSideA, accuSideB)
        case curCell +: rest => {

            val (cornerCoordA, cornerCoordB) = CornerCoord.twoSides(curCell.mazeChar)
            val curCellCornerCoordA: Set[CornerCoord] = cornerCoordA.map(cc => cc forCoord curCell.coord)
            val curCellCornerCoordB: Set[CornerCoord] = cornerCoordB.map(cc => cc forCoord curCell.coord)

            (accuSideA, accuSideB) match {
                case (a, b) if a.isEmpty && b.isEmpty => sideWalker(rest, curCellCornerCoordA, curCellCornerCoordB)

                case (a, b) if !(curCellCornerCoordA intersect a).isEmpty || !(curCellCornerCoordB intersect b).isEmpty =>
                    sideWalker(rest, a ++ curCellCornerCoordA, b ++ curCellCornerCoordB)

                case (a, b) if !(curCellCornerCoordA intersect b).isEmpty || !(curCellCornerCoordB intersect a).isEmpty =>
                    sideWalker(rest, a ++ curCellCornerCoordB, b ++ curCellCornerCoordA)

                case _ => {
                    println("Side walk ERROR!!!")
                    (Set(), Set())
                }
            }
        }
    }

    @tailrec
    def cellsOrderedInCycle(remainingCells: Set[Cell], curCell: Cell, accu: Vector[Cell] = Vector()): Vector[Cell] = {
        if (remainingCells.isEmpty) accu :+ curCell
        else {
            val curCellMoves = Coord.moves(curCell.mazeChar).map(m => curCell.coord + m)
            val nextCell: Option[Cell] = remainingCells.find(r => curCellMoves.contains(r.coord) && !(accu contains r)) // not optimal

            if (nextCell.isDefined)
                cellsOrderedInCycle(remainingCells - curCell, nextCell.get, accu :+ curCell)
            else
                accu :+ curCell
        }
    }

    @tailrec
    def spread(curCoordFront: Set[Coord], isOutside: Boolean = false, accuCoords: Set[Coord] = Set()): (Boolean, Set[Coord]) = {
        if (curCoordFront.isEmpty)
            (isOutside, accuCoords)
        else {
            val outOfBoundsFront: Set[Coord] = curCoordFront.filter(_.outOfBounds)
            val newIsOutside: Boolean = if (!isOutside) !outOfBoundsFront.isEmpty else isOutside
            val newFront = (curCoordFront -- outOfBoundsFront).flatMap(f => Coord.allDirections.map(d => d + f)) -- walkedCoords -- accuCoords -- curCoordFront

            spread(newFront, newIsOutside, accuCoords ++ curCoordFront -- outOfBoundsFront)
        }
    }


    // Part One
    val (startX: Int, startY: Int) = (0 to sizeY - 1).flatMap(y => (0 to sizeX - 1).map(x => (x, y))).find { case ((x, y)) => mazeGrid(y)(x) == 'S' }.get

    val startCoord: Coord = Coord(startX, startY)
    val startCell: Cell = Cell('S', startCoord, 0)
    val walkedCells: Set[Cell] = mazeWalker(Set(startCell))
    val maxCell = walkedCells.map(_.step).max
    println(s"Number of steps from Start to farthest point in Loop is $maxCell")


    // Part Two

    // replace S with actual
    val walkedCoords: Set[Coord] = walkedCells.map(_.coord)
    val walkedCoordsAfterS: Set[Coord] = walkedCells.filter(_.step == 1).map(_.coord)
    val sFootprint = startCell.neighbourCoords.filter(walkedCoordsAfterS contains _).map(n => n - startCell.coord)
    val actualSChar: Char = Coord.moves.map { case (a, b) => b -> a }(sFootprint)
    val startCellActual: Cell = Cell(actualSChar, startCoord, 0)
    val updatedWalkedCells: Vector[Cell] = (walkedCells - startCell + startCellActual).toVector.sortBy(_.step) ///
    println(s"The actual char for S is '$actualSChar'")

    val cellsOrdered: Vector[Cell] = cellsOrderedInCycle(updatedWalkedCells.toSet, startCellActual)
    val (sideCornersA: Set[CornerCoord], sideCornersB: Set[CornerCoord]) = sideWalker(cellsOrdered)

    val sideCoordsA: Set[Coord] = sideCornersA.flatMap(a => a.surroundingCoords) -- walkedCoords
    val sideCoordsB: Set[Coord] = sideCornersB.flatMap(b => b.surroundingCoords) -- walkedCoords

    val (isOutSideA, sideAllCoordsA) = spread(sideCoordsA: Set[Coord])
    val (isOutSideB, sideAllCoordsB) = spread(sideCoordsB: Set[Coord])

    (isOutSideA, sideAllCoordsA, isOutSideB, sideAllCoordsB) match {
        case (false, coordsA, _, _) => println(s"The number of enclosed tiles is ${coordsA.size}")
        case (_, _, false, coordsB) => println(s"The number of enclosed tiles is ${coordsB.size}")
        case _ => println("Error! Enclosed tiles not found!")
    }
}