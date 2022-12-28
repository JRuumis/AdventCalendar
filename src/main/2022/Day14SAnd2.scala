import scala.annotation.tailrec

object Day14SAnd2 extends App {

    val rockPathsRaw = scala.io.Source.fromFile("./Sources/Day14Sand.txt").getLines().toVector

    type DimCoord = Int

    case class Coord(x: DimCoord, y: DimCoord)

    val rockPathFromToCoords: Vector[Vector[Coord]] = rockPathsRaw
        .map(_.split(" -> ")
            .toVector.map(cr => cr.split(","))
            .map { case (Array(x, y)) => Coord(x.toInt, y.toInt) })

    @tailrec
    def fillGaps(coords: Vector[Coord], accu: Vector[Coord] = Vector()): Vector[Coord] = coords match {
        case from +: to +: rest => {

            //println(s"from ${from} -> ${to} to"  )

            val dy = to.y - from.y
            val dx = to.x - from.x

            fillGaps(to +: rest, accu ++ ((0 to dy by (if (dy < 0) -1 else 1)).flatMap(yy => (0 to dx by (if (dx < 0) -1 else 1)).map(xx => Coord(from.x + xx, from.y + yy))).toVector))

        }
        case _ => accu //.distinct
    }

    println(rockPathFromToCoords.mkString("\n"))

    val rockPathFromToCoordsFlat = rockPathFromToCoords.flatten
    val yMin: DimCoord = 0
    val yMax: DimCoord = rockPathFromToCoordsFlat.map(_.y).max
    val xMin: DimCoord = rockPathFromToCoordsFlat.map(_.x).min
    val xMax: DimCoord = rockPathFromToCoordsFlat.map(_.x).max

    val yLen = yMax - yMin
    val xLen = xMax - xMin

    val rockPathFromToCoordsFilled: Vector[Vector[Coord]] = rockPathFromToCoords.map(fillGaps(_))
    val rockPathFromToCoordsFilledFlatSet: Set[Coord] = rockPathFromToCoordsFilled.flatten.toSet

    val sandSource: Coord = Coord(500, 0)

    //println(rockPathFromToCoordsFilled.mkString("\n"))

    val startGrid: Vector[Vector[Char]] = (yMin to yLen).map(yy => (xMin to xMax).map { xx =>
        Coord(xx, yy) match {
            case c if c == sandSource => '+'
            case c if rockPathFromToCoordsFilledFlatSet contains c => '#'
            case _ => '.'
        }
    }.toVector).toVector

    def updateGrid[A](grid: Vector[Vector[A]], c: Coord, newVal: A): Vector[Vector[A]] = {
        grid.updated(c.y, grid(c.y).updated(c.x, newVal))
    }

    println(startGrid.map(_.mkString).mkString("\n"))

    //println(startGrid(0).length)


    val xMaxx = startGrid(0).length - 1
    val yMaxx = yMax

    // fill in Abyss
    def fillAbyss(currentCoord: Coord, accuGrid: Vector[Vector[Char]]): Vector[Vector[Char]] = {

        //println(s"current coord: ${currentCoord}")

        currentCoord match {
            case c if currentCoord.x > xMaxx =>
                accuGrid

            case c if accuGrid(currentCoord.y)(currentCoord.x) == '#' || currentCoord.y < 0 =>
                fillAbyss(Coord(currentCoord.x + 1, yMaxx), accuGrid)

            case c if accuGrid(currentCoord.y)(currentCoord.x) == '.' =>
                fillAbyss(Coord(currentCoord.x, currentCoord.y - 1), updateGrid(accuGrid, currentCoord, 'A'))
        }
    }

    val startGridWithAbyss: Vector[Vector[Char]] = fillAbyss(Coord(0, yMaxx), startGrid)
    println(startGridWithAbyss.map(_.mkString).mkString("\n"))

    println("--------------------")

    def dropSand(curSandCoord: Coord, startCoord: Coord, accuGrid: Vector[Vector[Char]], bottomY: DimCoord, maxxxxX: DimCoord, hitBottom: Boolean = false): Vector[Vector[Char]] = {

        //println(curSandCoord)
        //println(accuGrid.map(_.mkString).mkString("\n"))

        curSandCoord match {
            case sc if sc.y >= bottomY || sc.x < 0 || sc.x > maxxxxX || accuGrid(startCoord.y)(startCoord.x) == 'o' /*|| accuGrid(sc.y)(sc.x) == 'A'*/ =>
                accuGrid

            case sc if accuGrid(sc.y + 1)(sc.x) == '.' =>
                dropSand(Coord(sc.x, sc.y + 1), startCoord, accuGrid, bottomY, maxxxxX, hitBottom) // straight down

            case sc if (accuGrid(sc.y + 1)(sc.x) == '#' || accuGrid(sc.y + 1)(sc.x) == 'o') && (sc.x == 0 || (sc.x > 0 && accuGrid(sc.y + 1)(sc.x - 1) == '.')) =>
                dropSand(Coord(sc.x - 1, sc.y + 1), startCoord, accuGrid, bottomY, maxxxxX, hitBottom)

            case sc if (accuGrid(sc.y + 1)(sc.x) == '#' || accuGrid(sc.y + 1)(sc.x) == 'o') && (sc.x == maxxxxX || (sc.x < maxxxxX && accuGrid(sc.y + 1)(sc.x + 1) == '.')) =>
                dropSand(Coord(sc.x + 1, sc.y + 1), startCoord, accuGrid, bottomY, maxxxxX, hitBottom)

            case sc if (accuGrid(sc.y + 1)(sc.x) == '#' || accuGrid(sc.y + 1)(sc.x) == 'o') || (hitBottom && sc.y + 1 == bottomY) =>
                dropSand(startCoord, startCoord, updateGrid(accuGrid, sc, 'o'), bottomY, maxxxxX, hitBottom)

        }


    }


    val sss1 = Coord(sandSource.x - xMin, sandSource.y)

    val maxGrid = dropSand(sss1, sss1, startGrid, yMaxx, xMaxx)
    //val maxGrid = dropSand( Coord(sandSource.x - xMin, sandSource.y + 1), startGridWithAbyss )

    println("==================")
    println(maxGrid.map(_.mkString).mkString("\n"))


    val xxx = maxGrid.flatMap(v => v.map(c => if (c == 'o') 1 else 0)).sum
    println(xxx)




    // part 2

    val xAdjust = 1000
    val yMaxx2 = yMaxx + 2
    val xMaxx2 = 2000

    val wideGrid: Vector[Vector[Char]] = (0 to yMaxx + 2).map(yy => (0 to xMaxx2).map { xx =>
        (xx, yy) match {
            case (x, y) if y == yMaxx + 2 => '#'
            case (x, y) if x - xAdjust < 0 || x - xAdjust > xMaxx || y == yMaxx + 1 => '.'
            case (x, y) => startGrid(y)(x - xAdjust)
        }
    }.toVector).toVector


    println("==================")
    println(wideGrid.map(_.mkString).mkString("\n"))


    val sss2 = Coord(sandSource.x - xMin + xAdjust, sandSource.y)

    val maxGrid2 = dropSand(sss2, sss2, wideGrid, yMaxx2, xMaxx2, true)

    println("==================")
    println(maxGrid2.map(_.mkString).mkString("\n"))


    val xxx2 = maxGrid2.flatMap(v => v.map(c => if (c == 'o') 1 else 0)).sum

    println(1)
    println(xxx2)


}
