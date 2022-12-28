import scala.annotation.tailrec
import scala.util.matching.Regex

object Day22Map2 extends App {

    // PROD
    val mapInputRaw: Vector[String] = scala.io.Source.fromFile("./Sources/Day22Map.txt").getLines().toVector
    val edgeLength = 50

    // face name / face y / face x
    val faces = Vector(
        ('A', 0, 1),
        ('B', 0, 2),
        ('C', 1, 1),
        ('D', 2, 0),
        ('E', 2, 1),
        ('F', 3, 0)
    )

    // from face, leave direction,    to face, entry direction,      is mirrored
    val moveFaces = Vector(
        ('A', 'L', 'D', 'L', true),
        ('A', 'U', 'F', 'L', false),
        ('B', 'U', 'F', 'D', false),
        ('B', 'R', 'E', 'R', true),
        ('B', 'D', 'C', 'R', false),
        ('C', 'L', 'D', 'U', false),
        ('C', 'R', 'B', 'D', false),
        ('D', 'U', 'C', 'L', false),
        ('D', 'L', 'A', 'L', true),
        ('E', 'R', 'B', 'R', true),
        ('E', 'D', 'F', 'R', false),
        ('F', 'L', 'A', 'U', false),
        ('F', 'D', 'B', 'U', false),
        ('F', 'R', 'E', 'D', false)
    )


    /*
    // TEST
    val mapInputRaw: Vector[String] = scala.io.Source.fromFile("./Sources/Day22Map_TEST.txt").getLines().toVector
    val edgeLength = 4

    // face name / face y / face x
    val faces = Vector(
        ('A',0,2),
        ('B',1,0),
        ('C',1,1),
        ('D',1,2),
        ('E',2,2),
        ('F',2,3)
    )

    // from face, leave direction,    to face, entry direction,      is mirrored
    val moveFaces = Vector(
        ('A','L','C','U', false),
        ('A','U','B','U', true),
        ('A','R','F','R', true),
        ('B','U','A','U', true),
        ('B','L','F','D', true),
        ('B','D','E','D', true),
        ('C','U','A','L', false),
        ('C','D','E','L', true),
        ('D','R','F','U', true),
        ('E','L','C','D', true),
        ('E','D','B','D', true),
        ('F','U','D','R', true),
        ('F','R','A','R', true),
        ('F','D','B','L', true)
    )


 */


    val mapRaw = mapInputRaw.slice(0, mapInputRaw.length - 2)
    val directionsRaw = mapInputRaw.last

    val mapRowPattern: Regex = """( *)([\.#]+)""".r
    val directionDigitPattern: Regex = """([0-9]+)(.*)""".r
    val directionLRPattern: Regex = """([LR])(.*)""".r

    val mapParsed = mapRaw.map(m => m match {
        case mapRowPattern(a, b) => (a, b)
    })

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

        def opposite: Direction = d match {
            case 'L' => Direction('R')
            case 'R' => Direction('L')
            case 'U' => Direction('D')
            case 'D' => Direction('U')
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

    case class Face(s: Char)

    val coordFacesMap: Map[Coord, Face] = faces.map { case (a, b, c) => Coord(b, c) -> Face(a) }.toMap
    val facesCoordMap: Map[Face, Coord] = faces.map { case (a, b, c) => Face(a) -> Coord(b, c) }.toMap

    val moveFacesMap: Map[(Face, Direction), (Face, Direction, Boolean)] = moveFaces.map { case (s1, d1, s2, d2, inv) => (Face(s1), Direction(d1)) -> (Face(s2), Direction(d2), inv) }.toMap

    case class Coord(y: Int, x: Int) {

        def moveByUnsafe(gridCoord: GridCoord): Coord = Coord(y + gridCoord.y, x + gridCoord.x)

        lazy val face: Option[Face] = {
            if (y < 0 || x < 0) {
                None
            } else {
                val cY: Int = y / edgeLength
                val cX: Int = x / edgeLength

                coordFacesMap.get(Coord(cY, cX))
            }
        }

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

        def move2(direction: Direction, grid: Vector[Vector[Char]]): (Coord, Direction) = {
            val moveToGridCoord: GridCoord = Move(direction)
            val moveTo: Coord = moveByUnsafe(moveToGridCoord)

            if (moveTo.face.isDefined) { // inside the grid
                if (Set('.', 'L', 'R', 'U', 'D') contains grid(moveTo.y)(moveTo.x))
                    (moveTo, direction)
                else
                    (this, direction)
            } else { // outside the grid - need to calc new coord
                val (toFace: Face, toDirection: Direction, toInverted: Boolean) = moveFacesMap((face.get, direction))

                val faceCoordX = x % edgeLength
                val faceCoordY = y % edgeLength

                val toFaceCoord: Coord = facesCoordMap(toFace)

                val newBaseX: Int = toFaceCoord.x * edgeLength
                val newBaseY: Int = toFaceCoord.y * edgeLength

                val (newFaceX, newFaceY) = (direction, toDirection, toInverted) match {

                    case (Direction('L'), Direction('L'), inv) => (faceCoordX, if (inv) edgeLength - 1 - faceCoordY else faceCoordY)
                    case (Direction('L'), Direction('R'), inv) => (edgeLength - 1 - faceCoordX, if (inv) edgeLength - 1 - faceCoordY else faceCoordY)
                    case (Direction('L'), Direction('U'), inv) => (if (inv) edgeLength - 1 - faceCoordY else faceCoordY, faceCoordX)
                    case (Direction('L'), Direction('D'), inv) => (if (inv) edgeLength - 1 - faceCoordY else faceCoordY, edgeLength - 1 - faceCoordX)

                    case (Direction('R'), Direction('L'), inv) => (edgeLength - 1 - faceCoordX, if (inv) edgeLength - 1 - faceCoordY else faceCoordY)
                    case (Direction('R'), Direction('R'), inv) => (faceCoordX, if (inv) edgeLength - 1 - faceCoordY else faceCoordY)
                    case (Direction('R'), Direction('U'), inv) => (if (inv) edgeLength - 1 - faceCoordY else faceCoordY, edgeLength - 1 - faceCoordX)
                    case (Direction('R'), Direction('D'), inv) => (if (inv) edgeLength - 1 - faceCoordY else faceCoordY, faceCoordX)

                    case (Direction('U'), Direction('L'), inv) => (faceCoordY, if (inv) edgeLength - 1 - faceCoordX else faceCoordX)
                    case (Direction('U'), Direction('R'), inv) => (edgeLength - 1 - faceCoordY, if (inv) edgeLength - 1 - faceCoordX else faceCoordX)
                    case (Direction('U'), Direction('U'), inv) => (if (inv) edgeLength - 1 - faceCoordX else faceCoordX, faceCoordY)
                    case (Direction('U'), Direction('D'), inv) => (if (inv) edgeLength - 1 - faceCoordX else faceCoordX, edgeLength - 1 - faceCoordY)

                    case (Direction('D'), Direction('L'), inv) => (edgeLength - 1 - faceCoordY, if (inv) edgeLength - 1 - faceCoordX else faceCoordX)
                    case (Direction('D'), Direction('R'), inv) => (faceCoordY, if (inv) edgeLength - 1 - faceCoordX else faceCoordX)
                    case (Direction('D'), Direction('U'), inv) => (if (inv) edgeLength - 1 - faceCoordX else faceCoordX, edgeLength - 1 - faceCoordY)
                    case (Direction('D'), Direction('D'), inv) => (if (inv) edgeLength - 1 - faceCoordX else faceCoordX, faceCoordY)
                }

                val newX = newBaseX + newFaceX
                val newY = newBaseY + newFaceY

                if (Set('.', 'L', 'R', 'U', 'D') contains grid(newY)(newX))
                    (Coord(newY, newX), toDirection.opposite)
                else
                    (this, direction)
            }
        }

    }


    def updateGrid(grid: Vector[Vector[Char]], coord: Coord, newValue: Char): Vector[Vector[Char]] = {
        grid.updated(coord.y, grid(coord.y).updated(coord.x, newValue))
    }

    val startCoord: Coord = Coord(0, xMinMax(0)._1)


    def iterateStep(remainingSteps: Int, currentDirection: Direction, currentCoord: Coord, currentGrid: Vector[Vector[Char]]): (Coord, Vector[Vector[Char]], Direction) = {
        if (remainingSteps == 0)
            (currentCoord, currentGrid, currentDirection)
        else {
            //val newCoord = currentCoord.move(currentDirection, currentGrid)
            val (newCoord: Coord, newDir: Direction) = currentCoord.move2(currentDirection, currentGrid)
            val newGrid = updateGrid(currentGrid, newCoord, newDir.d)

            iterateStep(remainingSteps - 1, newDir, newCoord, newGrid)
        }
    }


    def iterate(currentDirection: Direction, currentCoord: Coord, directions: Vector[Command], currentGrid: Vector[Vector[Char]]): (Coord, Vector[Vector[Char]]) = {

        //println
        //println("-- IN ITERATE --")
        //println(currentGrid.map(_.mkString).mkString("\n"))

        directions match {
            case Vector() => (currentCoord, currentGrid)

            case (trn@Turn(t)) +: rest => {
                val newDirection: Direction = currentDirection.turn(trn)
                iterate(newDirection, currentCoord, rest, updateGrid(currentGrid, currentCoord, newDirection.d))
            }

            case Steps(s) +: rest => {
                val (newCoord, newGrid, newDir) = iterateStep(s, currentDirection, currentCoord, currentGrid)
                iterate(newDir, newCoord, rest, newGrid)
            }
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
