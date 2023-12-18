package jruumis.adventofcode.year2022

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day22Map extends App {

    val mapInputRaw: Vector[String] = scala.io.Source.fromFile("./Sources/Day22Map_TEST.txt").getLines().toVector

    val mapRaw = mapInputRaw.slice(0, mapInputRaw.length - 2)
    val directionsRaw = mapInputRaw.last

    //println(mapInputRaw)
    //println(mapRaw)
    //println(directionsRaw)

    val mapRowPattern: Regex = """( *)([\.#]+)""".r
    val directionDigitPattern: Regex = """([0-9]+)(.*)""".r
    val directionLRPattern: Regex = """([LR])(.*)""".r

    val mapParsed = mapRaw.map(m => m match {
        case mapRowPattern(a, b) => (a, b)
    })

    //println(mapParsed.mkString("\n"))

    abstract class Command

    case class Steps(s: Int) extends Command

    case class Turn(t: Char) extends Command

    @tailrec
    def getNextDir(dirRaw: String, accu: Vector[Command] = Vector()): Vector[Command] = dirRaw match {
        case "" => accu
        case directionDigitPattern(c, rest) => getNextDir(rest, accu :+ Steps(c.toInt))
        case directionLRPattern(c, rest) => getNextDir(rest, accu :+ Turn(c.head))
    }

    val directions: Vector[Command] = getNextDir(directionsRaw)
    println(directions)

    val xMinMax: Vector[(Int, Int)] = mapParsed.map { case (a, b) => (a.length, a.length + b.length - 1) }
    println(s"X min-,ax: ${xMinMax}")


    val mapConcat: Vector[String] = mapParsed.map { case (a, b) => a + b }
    val maxRowLength: Int = mapConcat.map(_.length).max

    val inputMap: Vector[Vector[Char]] = mapConcat.map(_.padTo(maxRowLength, ' ').toVector)
    //println(inputMap.map(_.mkString).mkString("\n"))

    val mapPaddedTransposed = inputMap.transpose
    val yMin: Vector[Int] = mapPaddedTransposed.map(a => a.indexWhere(c => c == '.' || c == '#'))
    val yMax: Vector[Int] = mapPaddedTransposed.map(a => a.length - a.reverse.indexWhere(c => c == '.' || c == '#') - 1)
    val yMinMax: Vector[(Int, Int)] = yMin zip yMax

    println(s"Y min-max: ${yMinMax}")

    case class Direction(d: Char) {
        def turn(t: Turn) = (t, d) match {
            case (Turn('R'), 'U') => Direction('R')
            case (Turn('R'), 'R') => Direction('D')
            case (Turn('R'), 'D') => Direction('L')
            case (Turn('R'), 'L') => Direction('U')
            case (Turn('L'), 'U') => Direction('L')
            case (Turn('L'), 'R') => Direction('U')
            case (Turn('L'), 'D') => Direction('R')
            case (Turn('L'), 'L') => Direction('D')
        }
    }

    abstract class GridCoord {
        val y: Int
        val x: Int
    }

    case class Move(dir: Direction) extends GridCoord {
        val y: Int = dir.d match {
            case 'U' => -1
            case 'D' => 1
            case _ => 0
        }
        val x: Int = dir.d match {
            case 'L' => -1
            case 'R' => 1
            case _ => 0
        }
    }

    case class Coord(y: Int, x: Int) {

        def moveByUnsafe(gridCoord: GridCoord): Coord = Coord(y + gridCoord.y, x + gridCoord.x)

        def moveBy(gridCoord: GridCoord, grid: Vector[Vector[Char]]): Coord = {
            val moveTo = moveByUnsafe(gridCoord)

            val yAdjusted: Int = {
                if (moveTo.y == y) moveTo.y
                else if (moveTo.y > yMinMax(moveTo.x)._2) yMinMax(moveTo.x)._1
                else if (moveTo.y < yMinMax(moveTo.x)._1) yMinMax(moveTo.x)._2
                else moveTo.y
            }

            val xAdjusted: Int = {
                if (moveTo.x == x) moveTo.x
                else if (moveTo.x > xMinMax(moveTo.y)._2) xMinMax(moveTo.y)._1
                else if (moveTo.x < xMinMax(moveTo.y)._1) xMinMax(moveTo.y)._2
                else moveTo.x
            }

            if (Set('.', 'L', 'R', 'U', 'D') contains grid(yAdjusted)(xAdjusted) /*== '.'*/ )
                Coord(yAdjusted, xAdjusted)
            else
                this
        }

        def move(dir: Direction, grid: Vector[Vector[Char]]): Coord = {
            this.moveBy(Move(dir), grid)
        }

    }


    def updateGrid(grid: Vector[Vector[Char]], coord: Coord, newValue: Char): Vector[Vector[Char]] = {
        grid.updated(coord.y, grid(coord.y).updated(coord.x, newValue))
    }

    val startCoord: Coord = Coord(0, xMinMax(0)._1)


    def iterateStep(remainingSteps: Int, currentDirection: Direction, currentCoord: Coord, currentGrid: Vector[Vector[Char]]): (Coord, Vector[Vector[Char]]) = {
        if (remainingSteps == 0)
            (currentCoord, currentGrid)
        else {
            val newCoord = currentCoord.move(currentDirection, currentGrid)
            val newGrid = updateGrid(currentGrid, newCoord, currentDirection.d)

            iterateStep(remainingSteps - 1, currentDirection, newCoord, newGrid)
        }
    }


    def iterate(currentDirection: Direction, currentCoord: Coord, directions: Vector[Command], currentGrid: Vector[Vector[Char]]): (Coord, Vector[Vector[Char]]) = directions match {

        case Vector() => (currentCoord, currentGrid)

        case (trn@Turn(t)) +: rest => {
            val newDirection: Direction = currentDirection.turn(trn)
            iterate(newDirection, currentCoord, rest, updateGrid(currentGrid, currentCoord, newDirection.d))
        }

        case Steps(s) +: rest => {
            val (newCoord, newGrid) = iterateStep(s, currentDirection, currentCoord, currentGrid)
            iterate(currentDirection, newCoord, rest, newGrid)
        }
    }

    val startDirection = Direction('R')
    val startMap = updateGrid(inputMap, startCoord, startDirection.d)

    println("-- START --")
    println(startMap.map(_.mkString).mkString("\n"))


    val (lastCoord: Coord, walkedGrid: Vector[Vector[Char]]) = iterate(startDirection, startCoord, directions, startMap)

    println("-- FINISH --")
    println(walkedGrid.map(_.mkString).mkString("\n"))
    println(s"last Coord: ${lastCoord}")

    val gridValAtLastCoord: Char = walkedGrid(lastCoord.y)(lastCoord.x)
    println(s"Last direction: ${gridValAtLastCoord}")

    val password: Int = 1000 * (lastCoord.y + 1) + 4 * (lastCoord.x + 1) + {
        gridValAtLastCoord match {
            case 'R' => 0
            case 'D' => 1
            case 'L' => 2
            case 'U' => 3
        }
    }

    println(s"passwordL: ${password}")


}
