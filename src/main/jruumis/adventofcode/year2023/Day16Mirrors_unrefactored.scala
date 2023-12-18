package jruumis.adventofcode.year2023

import scala.annotation.tailrec

object Day16Mirrors_unrefactored extends App {

    //val inputRaw = scala.io.Source.fromFile("./Sources/2023/Day16Lava_TEST1.txt").getLines.toVector.map(_.toVector)
    val inputRaw = scala.io.Source.fromFile("./Sources/2023/Day16Lava.txt").getLines.toVector.map(_.toVector)

    case class Coord(x: Int, y: Int) {
        def +(other: Coord): Coord = Coord(x + other.x, y + other.y)
    }

    sealed trait Direction {
        val coord: Coord
        val opposite: Direction
    }

    case object Up extends Direction {
        override val coord = Coord(0,-1)
        override val opposite: Direction = Down
    }

    case object Down extends Direction {
        override val coord = Coord(0,1)
        override val opposite: Direction = Up
    }

    case object Left extends Direction {
        override val coord = Coord(-1,0)
        override val opposite: Direction = Right
    }

    case object Right extends Direction {
        override val coord = Coord(1,0)
        override val opposite: Direction = Left
    }

    case class LaserPathInCell(enterFrom: Direction, exitTo: Direction)

    def exits(cellChar: Char, enterFrom: Direction): Set[LaserPathInCell] = (cellChar, enterFrom) match {
        case ('.', Up) => Set(LaserPathInCell(Up, Down))
        case ('.', Down) => Set(LaserPathInCell(Down, Up))
        case ('.', Left) => Set(LaserPathInCell(Left, Right))
        case ('.', Right) => Set(LaserPathInCell(Right, Left))

        case ('-', Up) => Set(LaserPathInCell(Up, Left), LaserPathInCell(Up, Right))
        case ('-', Down) => Set(LaserPathInCell(Down, Left), LaserPathInCell(Down, Right))
        case ('-', Left) => Set(LaserPathInCell(Left, Right))
        case ('-', Right) => Set(LaserPathInCell(Right, Left))

        case ('|', Up) => Set(LaserPathInCell(Up, Down))
        case ('|', Down) => Set(LaserPathInCell(Down, Up))
        case ('|', Left) => Set(LaserPathInCell(Left, Up), LaserPathInCell(Left, Down))
        case ('|', Right) => Set(LaserPathInCell(Right, Up), LaserPathInCell(Right, Down))

        case ('/',Up) => Set(LaserPathInCell(Up, Left))
        case ('/',Down) => Set(LaserPathInCell(Down, Right))
        case ('/',Left) => Set(LaserPathInCell(Left, Up))
        case ('/',Right) => Set(LaserPathInCell(Right, Down))

        case ('\\',Up) => Set(LaserPathInCell(Up, Right))
        case ('\\',Down) => Set(LaserPathInCell(Down, Left))
        case ('\\',Left) => Set(LaserPathInCell(Left, Down))
        case ('\\',Right) => Set(LaserPathInCell(Right, Up))
    }

    case class Cell(cellChar: Char, laserPaths: Set[LaserPathInCell] = Set()) {
        def addPaths(newPaths: Set[LaserPathInCell]): Cell = Cell(cellChar, laserPaths ++ newPaths)
    }

    case class Grid(cells: Vector[Vector[Cell]]) {
        override def toString: String = cells.map(row => row.map(c => c.cellChar).mkString).mkString("\n")

        def toStringCells = cells.map(row => row.map(cell => cell.laserPaths.size).mkString(",")).mkString("\n")

        val energisedCells = cells.flatMap(row => row.map(cell => if(cell.laserPaths.size > 0) 1 else 0)).sum

        def isCoordInside(coord: Coord): Boolean = coord.x >= 0 && coord.y >= 0 && coord.x < cells(0).length && coord.y < cells.length

        @tailrec
        final def moveLaser(
                         currentFront: Set[LaserFront],
                         accuFront: Set[LaserFront] = Set(),
                         accuUpdatedCells: Vector[(Cell, Coord)] = Vector()
                     ): ( Set[LaserFront], Vector[(Cell, Coord)] ) = currentFront.toVector match {

            case Vector() => ( accuFront, accuUpdatedCells )

            case f +: rest if this.isCoordInside(f.coord) => {

                val curCell: Cell = cells(f.coord.y)(f.coord.x)

                val curPossiblePaths: Set[LaserPathInCell] = exits(curCell.cellChar, f.direction)
                val curAdditionalPaths: Set[LaserPathInCell] = curPossiblePaths -- curCell.laserPaths

                val exitDirections: Set[Direction] = curAdditionalPaths.map(p => p.exitTo)

                val curFront: Set[LaserFront] = exitDirections.map(d => LaserFront(d.opposite, f.coord + d.coord))

                val newAccuUpdatedCells: Vector[(Cell, Coord)] = if (curAdditionalPaths.isEmpty) accuUpdatedCells else {
                    accuUpdatedCells :+ (curCell.addPaths(curAdditionalPaths), f.coord)
                }

                moveLaser(rest.toSet, accuFront ++ curFront, newAccuUpdatedCells)
            }

            case f +: rest => moveLaser(rest.toSet, accuFront, accuUpdatedCells)
        }

        @tailrec
        final def updateGridFromFront(updatedCells: Vector[(Cell, Coord)], accuGrid: Grid = this): Grid = updatedCells match {
            case Vector() => accuGrid
            case (newCell, coord) +: rest => {
                val newCells: Vector[Vector[Cell]] = accuGrid.cells.updated(coord.y, accuGrid.cells(coord.y).updated(coord.x, newCell))

                updateGridFromFront(rest, Grid(newCells))
            }
        }
    }

    @tailrec
    def iterator(grid: Grid, curLaserFront: Set[LaserFront]): Grid = {

        val (newLaserFront: Set[LaserFront], updatedCells: Vector[(Cell, Coord)]) = grid.moveLaser(curLaserFront)
        //println("iter")
        val newGrid = grid.updateGridFromFront(updatedCells)

        if( newLaserFront.isEmpty )
            newGrid
        else
            iterator(newGrid, newLaserFront)
    }

    case class LaserFront(direction: Direction, coord: Coord)

    val startLaserFront: Set[LaserFront] = Set(LaserFront(Left, Coord(0,0)))

    val startGrid = Grid(inputRaw.map(_.map(ch => Cell(ch))))

    println(startGrid)

    val finishGrid = iterator(startGrid, startLaserFront)
    println(finishGrid.energisedCells)

    val gridMax: Int = startGrid.cells.size - 1



    val allStartFronts = (0 to gridMax).map(i => LaserFront(Left, Coord(0,i)) ) ++
                    (0 to gridMax).map(i => LaserFront(Right, Coord(gridMax,i)) ) ++
                    (0 to gridMax).map(i => LaserFront(Up, Coord(i,0)) ) ++
                    (0 to gridMax).map(i => LaserFront(Down, Coord(i,gridMax)) )

    val startFrontsIndexed = allStartFronts.zipWithIndex.toVector

    val allFinishGrids = startFrontsIndexed.map{case(startFront, index) => {
        print(s"processing start front ${index+1} of ${startFrontsIndexed.size})...")

        val startTime = java.time.Instant.now()

        val result = iterator(startGrid, Set(startFront))

        val finishTime = java.time.Instant.now()

        val duration = java.time.Duration.between(startTime, finishTime).toMillis
        println(s" duration: $duration ms")

        result
    }}

    val xxx = allFinishGrids.map(fin => fin.energisedCells)

    println(xxx)
    println(xxx.max)
}