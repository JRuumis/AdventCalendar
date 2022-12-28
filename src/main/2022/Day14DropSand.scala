import scala.annotation.tailrec

object Day14DropSand extends App {

    // -= Coordinates and Grid =-
    type DimCoord = Int

    case class Coord(x: DimCoord, y: DimCoord) {
        def up: Coord = Coord(x, y - 1)

        def down: Coord = Coord(x, y + 1)

        def left: Coord = Coord(x - 1, y)

        def right: Coord = Coord(x + 1, y)
    }

    case class Grid(
                       sandSource: Coord,
                       grid: Vector[Vector[Char]],
                       coordAdjustmentX: DimCoord,
                       minX: DimCoord,
                       maxX: DimCoord,
                       minY: DimCoord,
                       maxY: DimCoord,
                       settledSand: Vector[Coord]
                   ) {

        override def toString: String = grid.map(_.mkString).mkString("\n")

        private def updateGrid[A](grid: Vector[Vector[A]], c: Coord, newVal: A): Vector[Vector[A]] = {
            grid.updated(c.y, grid(c.y).updated(c.x, newVal))
        }

        def isInsideGrid(c: Coord): Boolean = c.x >= minX && c.x <= maxX && c.y >= minY && c.y <= maxY

        def getFromGrid(c: Coord, g: Vector[Vector[Char]] = grid): Char = g(c.y)(c.x)

        @tailrec
        final def dropSand(sand: Coord = sandSource, accuGrid: Vector[Vector[Char]] = grid, accuSettledSand: Vector[Coord] = Vector()): Grid = sand match {
            // off grid or source covered
            case s if !isInsideGrid(s.down) || getFromGrid(sandSource, accuGrid) == 'o' =>
                Grid(sandSource, accuGrid, coordAdjustmentX, minX, maxX, minY, maxY, accuSettledSand)

            // fall straight down
            case s if getFromGrid(s.down, accuGrid) == '.' =>
                dropSand(s.down, accuGrid, accuSettledSand)

            // fall down, left
            case s if ("#o" contains getFromGrid(s.down, accuGrid)) && (s.x == minX || (s.x > minX && getFromGrid(s.down.left, accuGrid) == '.')) =>
                dropSand(s.down.left, accuGrid, accuSettledSand)

            // fall down, right
            case s if ("#o" contains getFromGrid(s.down, accuGrid)) && (s.x == maxX || (s.x < maxX && getFromGrid(s.down.right, accuGrid) == '.')) =>
                dropSand(s.down.right, accuGrid, accuSettledSand)

            // settle
            case s if ("#o" contains getFromGrid(s.down, accuGrid)) =>
                dropSand(sandSource, updateGrid(accuGrid, s, 'o'), accuSettledSand :+ s)
        }

        def addRow(lineChar: Char): Grid = {
            val newMaxX: DimCoord = maxX + 2
            val newMaxY: DimCoord = maxY + 1
            val newGrid: Vector[Vector[Char]] = (grid.map { row => '.' +: row :+ '.' }) :+ (lineChar.toString * newMaxX).toVector

            Grid(sandSource.right, newGrid, coordAdjustmentX + 1, minX, newMaxX, minY, newMaxY, settledSand)
        }

        def nrOfSettledSand: Int = settledSand.size
    }

    object Grid {
        def apply(sandSource: Coord, rocks: Set[Coord]): Grid = {

            val foundMinX = rocks.map { case (Coord(x, _)) => x }.min
            val foundMaxX = rocks.map { case (Coord(x, _)) => x }.max
            val foundMaxY = rocks.map { case (Coord(_, y)) => y }.max

            val requiredDimX: DimCoord = Vector(foundMaxY, (foundMaxX - sandSource.x), (sandSource.x - foundMinX)).max
            val requiredMaxX = 1 + requiredDimX * 2
            val adjustmentX = sandSource.x - requiredDimX
            val adjustedSandSource = Coord(sandSource.x - adjustmentX, sandSource.y)

            val newGrid: Vector[Vector[Char]] = (0 to foundMaxY).map(y => (0 to requiredMaxX).map { x =>
                Coord(x + adjustmentX, y) match {
                    case c if rocks contains c => '#'
                    case c if c == sandSource => '+'
                    case _ => '.'
                }
            }.toVector).toVector

            Grid(adjustedSandSource, newGrid, adjustmentX, 0, requiredMaxX, 0, foundMaxY, Vector())
        }
    }


    // -= MAIN =-
    val rockPathsRaw: Vector[String] = scala.io.Source.fromFile("./Sources/Day14Sand.txt").getLines().toVector

    val rockPathsFromToCoords: Vector[Vector[Coord]] = rockPathsRaw
        .map(_.split(" -> ").toVector
            .map(c => c.split(","))
            .map { case (Array(x, y)) => Coord(x.toInt, y.toInt) }
        )

    @tailrec
    def fillRockGapsBetweenFromTo(coords: Vector[Coord], accu: Vector[Coord] = Vector()): Vector[Coord] = coords match {
        case from +: to +: rest => {
            val moveToY = to.y - from.y
            val moveToX = to.x - from.x
            val dirY = if (moveToY < 0) -1 else 1
            val dirX = if (moveToX < 0) -1 else 1

            fillRockGapsBetweenFromTo(
                to +: rest,
                accu ++ ((0 to moveToY by dirY).flatMap(dy => (0 to moveToX by dirX).map(dx => Coord(from.x + dx, from.y + dy))).toVector)
            )
        }
        case _ => accu
    }

    val rockPathFromToCoordsFilled: Vector[Vector[Coord]] = rockPathsFromToCoords.map(fillRockGapsBetweenFromTo(_))
    val rockPathFromToCoordsFilledFlatSet: Set[Coord] = rockPathFromToCoordsFilled.flatten.toSet
    val sandSource: Coord = Coord(500, 0)
    val startGrid = Grid(sandSource, rockPathFromToCoordsFilledFlatSet)

    // -= Part 1 =-
    println(s"\n---= Starting Grid (Part 1) =---\n${startGrid}")
    val settledGrid = startGrid.dropSand()
    println(s"\n---= Grid with Settled Sand (Part 1) =---\n${settledGrid}")
    println(s"\nNumber of Settled Sands (Part 1): ${settledGrid.nrOfSettledSand}")

    // -= Part 2 =-
    val startGridPartTwo = startGrid.addRow('.').addRow('#')
    println(s"\n---= Starting Grid (Part 2) =---\n${startGridPartTwo}")
    val settledGridPartTwo = startGridPartTwo.dropSand()

    println(s"\n---= Grid with Settled Sand (Part 2) =---\n${settledGridPartTwo}")
    println(s"\nNumber of Settled Sands (Part 2): ${settledGridPartTwo.nrOfSettledSand}")
}
