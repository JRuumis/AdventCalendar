package jruumis.adventofcode.year2022

import scala.annotation.tailrec

object Day24Blizzard extends App {

    val blizzardRaw: Vector[Vector[Char]] = scala.io.Source.fromFile("./Sources/Day24Blizzard.txt").getLines().toVector.map(_.toVector)
    val blizzardWithoutLF = blizzardRaw.map(r => r.slice(1, r.length - 1))
    val gridBody: Vector[Vector[Char]] = blizzardWithoutLF.slice(1, blizzardWithoutLF.length - 1)

    val gridX: Int = gridBody(0).length
    val gridY: Int = gridBody.length
    val longCycleLength: Int = gridX * gridY // worst case scenario

    @tailrec
    def gcd(a: Int, b: Int): Int = b match {
        case 0 => a
        case n => gcd(b, a % b)
    }

    case class Coord(y: Int, x: Int) {
        def +(other: Coord): Coord = Coord(y + other.y, x + other.x)

        def up: Coord = this + Coord(-1, 0)

        def down: Coord = this + Coord(1, 0)

        def left: Coord = this + Coord(0, -1)

        def right: Coord = this + Coord(0, 1)

        def getMoves: Set[Coord] = Set(this, this.up, this.down, this.left, this.right)
    }

    case class Blizzard(coord: Coord, direction: Char) {
        val directionCoord: Coord = direction match {
            case '^' => Coord(-1, 0)
            case 'v' => Coord(1, 0)
            case '>' => Coord(0, 1)
            case '<' => Coord(0, -1)
        }

        def move(grid: Grid): Blizzard = {
            val nextCoord: Coord = (this.coord + directionCoord) match {
                case Coord(-1, x) => Coord(grid.sizeY - 1, x)
                case Coord(y, -1) => Coord(y, grid.sizeX - 1)
                case Coord(grid.sizeY, x) => Coord(0, x)
                case Coord(y, grid.sizeX) => Coord(y, 0)
                case c => c
            }

            Blizzard(nextCoord, direction)
        }
    }

    case class Grid(sizeY: Int, sizeX: Int, blizzards: Set[Blizzard]) {
        def next: Grid = Grid(sizeY, sizeX, blizzards.map(_.move(this)))

        def allGridCoords: Set[Coord] = (0 to sizeY - 1).flatMap(y => (0 to sizeX - 1).map(x => Coord(y, x))).toSet

        def emptyCoords: Set[Coord] = {
            allGridCoords -- blizzards.map(_.coord)
        }

        override def toString: String = (0 to sizeY - 1).map(y => (0 to sizeX - 1).map(x => {
            val blizzardsHere: Set[Blizzard] = blizzards.filter(_.coord == Coord(y, x))
            blizzardsHere.toVector match {
                case Vector() => '.'
                case i +: Vector() => i.direction
                case vec => (vec.length.toString.head)
            }
        }).mkString).mkString("\n")

    }

    val cycleLength: Int = gridX * gridY / gcd(gridX, gridY)
    val startCoord = Coord(-1, 0)
    val endCoord = Coord(gridY, gridX - 1)

    println(s"Cycle length: ${cycleLength}, start: ${startCoord}, finish: ${endCoord}")

    val startBlizzards: Set[Blizzard] = (0 to gridBody.length - 1).flatMap(y => (0 to gridBody(0).length - 1).map(x => (y, x, gridBody(y)(x)) match {
        case (_, _, '.') => None
        case (y, x, c) => Some(Blizzard(Coord(y, x), c))
    })).filter(_.isDefined).map(_.get).toSet

    val startGrid = Grid(gridY, gridX, startBlizzards)

    @tailrec
    def iterateGrid(stepsLeft: Int, currentGrid: Grid, accuGrids: Vector[Grid] = Vector()): Vector[Grid] = {
        if (stepsLeft == 0) accuGrids else {
            val nextGrid: Grid = currentGrid.next
            iterateGrid(stepsLeft - 1, nextGrid, accuGrids :+ nextGrid)
        }
    }

    println("Iterating grid...")
    val cycleGrids: Vector[Grid] = iterateGrid(cycleLength - 1, startGrid, Vector(startGrid))
    println("Grids acquired.")

    val emptyCoords: Vector[Set[Coord]] = cycleGrids.map(g => g.emptyCoords + startCoord + endCoord)

    @tailrec
    def iterateSearch(
                         destinations: Vector[Coord],
                         time: Int,
                         frontier: Set[Coord],
                         currentEmptyCoords: Vector[Set[Coord]] //currentEmptyBlizzardCoords: Vector[(Set[Coord], Set[Coord])]
                     ): Int = (currentEmptyCoords, destinations) match {

        case (_, curDest +: Vector()) if frontier contains curDest => time - 1

        case (curEmpty +: restEmpty, curDest +: restDest) if frontier contains curDest => {
            iterateSearch(restDest, time + 1, Set(curDest), restEmpty :+ curEmpty)
        }

        case _ if frontier.isEmpty => {
            println("ERROR!!! empty frontier")
            time - 1
        }
        case (curEmpty +: restEmpty, dest) => {
            val newFrontier: Set[Coord] = frontier.flatMap(_.getMoves) intersect curEmpty
            iterateSearch(dest, time + 1, newFrontier, restEmpty :+ curEmpty)
        }

    }

    println("Searching shortest time...")
    val shortestTime = iterateSearch(Vector(endCoord, startCoord, endCoord), 0, Set(startCoord), emptyCoords)
    println(s"Shortest time found: ${shortestTime}")
}
