import scala.annotation.tailrec

object Day10PipeMaze2_OLD2_working extends App {

    //val maze: Vector[Vector[Char]] = scala.io.Source.fromFile("./Sources/2023/Day10PipeMaze_TEST_SIMPLE.txt").getLines().toVector.map(_.toVector)
    //val maze: Vector[Vector[Char]] = scala.io.Source.fromFile("./Sources/2023/Day10PipeMaze_TEST_HARD.txt").getLines().toVector.map(_.toVector)

    //val maze: Vector[Vector[Char]] = scala.io.Source.fromFile("./Sources/2023/Day10PipeMaze_TEST_ENC1.txt").getLines().toVector.map(_.toVector)
    //val maze: Vector[Vector[Char]] = scala.io.Source.fromFile("./Sources/2023/Day10PipeMaze_TEST_ENC3.txt").getLines().toVector.map(_.toVector)
    //val maze: Vector[Vector[Char]] = scala.io.Source.fromFile("./Sources/2023/Day10PipeMaze_TEST_ENC2.txt").getLines().toVector.map(_.toVector)

    val maze: Vector[Vector[Char]] = scala.io.Source.fromFile("./Sources/2023/Day10PipeMaze.txt").getLines().toVector.map(_.toVector)

    //println(maze.mkString("\n"))
    //println

    val sizeX: Int = maze.head.size
    val sizeY: Int = maze.size

    case class Coord(x: Int, y: Int) {
        def +(other: Coord): Coord = Coord(this.x + other.x, this.y + other.y)
        def -(other: Coord): Coord = Coord(this.x - other.x, this.y - other.y)
        lazy val outOfBounds: Boolean = x < 0 || x >= sizeX || y < 0 || y >= sizeY
    }

    val upLeftCorner = CornerCoord(-0.5, -0.5)
    val upRightCorner = CornerCoord(0.5, -0.5)
    val downLeftCorner = CornerCoord(-0.5, 0.5)
    val downRightCorner = CornerCoord(0.5, 0.5)

    val allCorners: Set[CornerCoord] = Set(upLeftCorner, upRightCorner, downLeftCorner, downRightCorner)


    case class CornerCoord(x: Double, y: Double) {
        def forCoord(coord: Coord): CornerCoord = CornerCoord(this.x + coord.x, this.y + coord.y)

        lazy val surroundingCoords: Set[Coord] = allCorners.map(c => Coord( (this.x + c.x).toInt, (this.y + c.y).toInt ) )
    }



    def getMazeChar(coord: Coord): Option[Char] =
        if (coord.x < 0 || coord.y < 0 || coord.x >= sizeX || coord.y >= sizeY) None
        else Some(maze(coord.y)(coord.x))

    val upCoord = Coord(0,-1)
    val downCoord = Coord(0,1)
    val leftCoord = Coord(-1,0)
    val rightCoord = Coord(1,0)

    val upLeftCoord = Coord(-1,-1)
    val upRightCoord = Coord(1,-1)
    val downLeftCoord = Coord(-1, 1)
    val downRightCoord = Coord(1, 1)

    val allDirections: Set[Coord] = Set(
        upCoord,
        downCoord,
        leftCoord,
        rightCoord,

        upLeftCoord,
        upRightCoord,
        downLeftCoord,
        downRightCoord
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


    // Set - side A
    // Set - side B
    val twoSides: Map[Char, (Set[Coord], Set[Coord])] = Map(
        'F' -> (Set(downLeftCoord, leftCoord, upLeftCoord, upCoord, upRightCoord), Set(downRightCoord)),
        '7' -> (Set(upLeftCoord, upCoord, upRightCoord, rightCoord, downRightCoord), Set(downLeftCoord)),
        'J' -> (Set(upRightCoord, rightCoord, downRightCoord, downCoord, downLeftCoord), Set(upLeftCoord)),
        'L' -> (Set(upLeftCoord, leftCoord, downLeftCoord, downCoord, downRightCoord), Set(upRightCoord)),
        '-' -> (Set(upLeftCoord, upCoord, upRightCoord), Set(downLeftCoord, downCoord, downRightCoord)),
        '|' -> (Set(upRightCoord, rightCoord, downRightCoord), Set(upLeftCoord, leftCoord, downLeftCoord))
    )



    // Set - side A
    // Set - side B
    val twoSides2: Map[Char, (Set[CornerCoord], Set[CornerCoord])] = Map(
        'F' -> (Set(downLeftCorner, upLeftCorner, upRightCorner), Set(downRightCorner)),
        '7' -> (Set(upLeftCorner, upRightCorner, downRightCorner), Set(downLeftCorner)),
        'J' -> (Set(upRightCorner, downRightCorner, downLeftCorner), Set(upLeftCorner)),
        'L' -> (Set(upLeftCorner, downLeftCorner, downRightCorner), Set(upRightCorner)),
        '-' -> (Set(upLeftCorner, upRightCorner), Set(downLeftCorner, downRightCorner)),
        '|' -> (Set(upLeftCorner, downLeftCorner), Set(upRightCorner, downRightCorner))
    )


    case class Cell(mazeChar: Char, coord: Coord, step: Int) {
        lazy val neighbourCoords: Set[Coord] = {
            moves(mazeChar).map(moveCoord => coord + moveCoord)
        }

        def isConnectedTo(other: Cell): Boolean = {
            val thisConnectedToOther: Boolean = (moves(this.mazeChar).map(_ + this.coord).contains(other.coord))
            val otherConnectedToThis: Boolean = (moves(other.mazeChar).map(_ + other.coord) contains this.coord)

            thisConnectedToOther && otherConnectedToThis
        }


    }


    val (startX, startY) = (0 to sizeY-1).flatMap(y => (0 to sizeX-1).map(x => (x,y))).find {case((x,y)) => maze(y)(x) == 'S'}.get


    val startCoord: Coord = Coord(startX, startY)
    val startCell: Cell = Cell('S', startCoord, 0)



    @tailrec
    def mazeWalker(frontCells: Set[Cell], visitedCells: Set[Cell] = Set(), visitedCoords: Set[Coord] = Set(), currentDepth: Int = 0): Set[Cell] = {
        if (frontCells.isEmpty) visitedCells
        else {

            val nextFrontCells: Set[Cell] = frontCells.flatMap(frontCell => {
                frontCell.neighbourCoords
                    .map(coord => (coord, getMazeChar(coord)))
                    .filter { case (_, char) => char.isDefined } // valid coordinates - not outside the maze
                    .filter { case (coord, _) => !visitedCoords.contains(coord) } // exclude already visited coordinates
                    .map { case (coord, Some(char)) => Cell(char, coord, currentDepth + 1) }
                    .filter(_ isConnectedTo frontCell) // exclude neighbours that are not properly connected
            })


            mazeWalker(nextFrontCells, visitedCells ++ frontCells, visitedCoords ++ frontCells.map(_.coord), currentDepth+1)
        }
    }


    val walkedCells: Set[Cell] = mazeWalker(Set(startCell))
    //println(walkedCells.toVector.sortBy(_.step).mkString("\n"))

    val maxCell = walkedCells.map(_.step).max
    println(maxCell)


    def getGridFromWalk(walkedCells: Set[Cell]): Vector[Vector[Char]] = {
        //val minX = walkedCells.map(_.coord.x).min
        //val maxX = walkedCells.map(_.coord.x).max
        //val minY = walkedCells.map(_.coord.y).min
        //val maxY = walkedCells.map(_.coord.y).max

        (0 to sizeY-1).toVector
            .map(y => (0 to sizeX-1).toVector.map(x => {
                val foundCell = walkedCells.find(_.coord == Coord(x,y))

                if(foundCell.isEmpty) '.'
                else foundCell.get.mazeChar
            }))
    }

    val updatedMazeWithS: Vector[Vector[Char]] = getGridFromWalk(walkedCells)
    println(updatedMazeWithS.map(_.mkString).mkString("\n"))



    //val startCoord: Coord = Coord(startX, startY)
    //val startCell: Cell = Cell('S', startCoord, 0)

    val walkedCoords: Set[Coord] = walkedCells.map(_.coord) ///
    val walkedCoordsAfterS: Set[Coord] = walkedCells.filter(_.step == 1).map(_.coord)

    val sFootprint = startCell.neighbourCoords.filter(walkedCoordsAfterS contains _).map(n => n - startCell.coord)

    val actualSChar: Char = moves.map { case (a, b) => b -> a }(sFootprint)
    println(s"The actual char for S is '$actualSChar'")

    val newStartCell: Cell = Cell(actualSChar, startCoord, 0)
    val updatedWalkedCells: Vector[Cell] = (walkedCells - startCell + newStartCell).toVector.sortBy(_.step) ///

    //println(walkedCells.toVector.sortBy(_.step))
    //println(updatedWalkedCells)


    /////  updatedWalkedCells

    @tailrec
    def sideWalker2(cellsInPath: Vector[Cell], accuSideA: Set[CornerCoord] = Set(), accuSideB: Set[CornerCoord] = Set()): (Set[CornerCoord],Set[CornerCoord]) = cellsInPath match {
        case Vector() => (accuSideA, accuSideB)
        case curCell +: rest => {

            val (cornerCoordA,cornerCoordB) = twoSides2(curCell.mazeChar)
            val curCellCornerCoordA: Set[CornerCoord] = cornerCoordA.map(cc => cc forCoord curCell.coord)
            val curCellCornerCoordB: Set[CornerCoord] = cornerCoordB.map(cc => cc forCoord curCell.coord)

            (accuSideA, accuSideB) match {
                case (a, b) if a.isEmpty && b.isEmpty => sideWalker2(rest, curCellCornerCoordA, curCellCornerCoordB)
                case (a, b) if !(curCellCornerCoordA intersect a).isEmpty => sideWalker2(rest, a ++ curCellCornerCoordA, b ++ curCellCornerCoordB)
                case (a, b) if !(curCellCornerCoordA intersect b).isEmpty => sideWalker2(rest, a ++ curCellCornerCoordB, b ++ curCellCornerCoordA)
                case (a, b) if !(curCellCornerCoordB intersect a).isEmpty => sideWalker2(rest, a ++ curCellCornerCoordB, b ++ curCellCornerCoordA)
                case (a, b) if !(curCellCornerCoordB intersect b).isEmpty => sideWalker2(rest, a ++ curCellCornerCoordA, b ++ curCellCornerCoordB)

                case _ => {
                    println("Side walk ERROR!!!")
                    (Set(), Set())
                }
            }

        }
    }




    @tailrec
    def sideWalker(walkedCells: Vector[Cell], accuSideA: Set[Coord] = Set(), accuSideB: Set[Coord] = Set()): (Set[Coord],Set[Coord]) = walkedCells match {
        case Vector() => (accuSideA, accuSideB)
        case curCell +: rest => {

            // A and B from current cell
            val (setA,setB) = twoSides(curCell.mazeChar)
            val curCoordSetA: Set[Coord] = setA.map(_ + curCell.coord)
            val curCoordSetB: Set[Coord] = setB.map(_ + curCell.coord)


            // A and B from neighbours
            val curMoves: Set[Coord] = moves(curCell.mazeChar)
            val curNeighbourCoords: Set[Coord] = curMoves.map(_ + curCell.coord)
            val curNeighbourCells: Vector[Cell] = updatedWalkedCells.filter(c => curNeighbourCoords contains c.coord)

            if (curNeighbourCells.size != 2) {
                println("Neighbour error!")
            }

            val neighbourSides: Vector[(Set[Coord], Set[Coord])] = curNeighbourCells.map(nc => {
                val (a: Set[Coord],b: Set[Coord]) = twoSides(nc.mazeChar)

                val aa: Set[Coord] = a.map(_ + nc.coord)
                val bb: Set[Coord] = b.map(_ + nc.coord)

                (aa,bb)
            })


            // intersect current AB with neighbours
            val (curSetA, curSetB) = neighbourSides.foldLeft( (Set(), Set()): (Set[Coord], Set[Coord]) )((a, b) => (a, b) match {
                case ((a1, b1), (a2, b2)) if !(a2 intersect curCoordSetA).isEmpty || !(b2 intersect curCoordSetB).isEmpty => (a1 ++ (a2 intersect curCoordSetA), b1 ++ (b2 intersect curCoordSetB))
                case ((a1, b1), (a2, b2)) if !(b2 intersect curCoordSetA).isEmpty || !(a2 intersect curCoordSetB).isEmpty => (a1 ++ (b2 intersect curCoordSetA), b1 ++ (a2 intersect curCoordSetB))
            })

            //val xxx = neighbourSides.foldLeft( (Set(), Set()): (Set[Coord], Set[Coord]) )()


            /*
            val xxxZZZ = neighbourSides.map{case(sa,sb) => {
                if( !(sa intersect curCoordSetA).isEmpty ) (sa intersect curCoordSetA, sb intersect curCoordSetB)
                else (sa intersect curCoordSetA, sb intersect curCoordSetB)
            }}

             */


            /*
            val xxxMerged = neighbourSides.foldLeft( (Set(), Set()): (Set[Coord], Set[Coord]) )( (a,b) => (a,b) match {
                case ( (a1,b1) , (a2,b2) ) if !(a1 intersect a2).isEmpty || !(b1 intersect b2).isEmpty => (a1 ++ a2, b2 ++ b2)
                case ( (a1,b1) , (a2,b2) ) if !(a1 intersect b2).isEmpty || !(b1 intersect a2).isEmpty => (a1 ++ b2, b1 ++ a2)
            } )

             */


            //val curCoordSetA: Set[Coord] = setA.map(_ + curCell.coord)
            //val curCoordSetB: Set[Coord] = setB.map(_ + curCell.coord)


            (accuSideA, accuSideB) match {
                case (a, b) if a.isEmpty && b.isEmpty => sideWalker(rest, curSetA, curSetB)
                case (a, b) if !(curSetA intersect a).isEmpty => sideWalker(rest, a ++ curSetA, b ++ curSetB)
                case (a, b) if !(curSetA intersect b).isEmpty => sideWalker(rest, a ++ curSetB, b ++ curSetA)
                case (a, b) if !(curSetB intersect a).isEmpty => sideWalker(rest, a ++ curSetB, b ++ curSetA)
                case (a, b) if !(curSetB intersect b).isEmpty => sideWalker(rest, a ++ curSetA, b ++ curSetB)

                case _ => {
                    println("Side walk ERROR!!!")
                    (Set(),Set())
                }
            }

            /*
            (accuSideA, accuSideB) match {
                case (a, b) if a.isEmpty && b.isEmpty => sideWalker(rest, curCoordSetA, curCoordSetB)
                case (a, b) if !(curCoordSetA intersect a).isEmpty => sideWalker(rest, a ++ curCoordSetA, b ++ curCoordSetB)
                case (a, b) if !(curCoordSetA intersect b).isEmpty => sideWalker(rest, a ++ curCoordSetB, b ++ curCoordSetA)
                case (a, b) if !(curCoordSetB intersect a).isEmpty => sideWalker(rest, a ++ curCoordSetB, b ++ curCoordSetA)
                case (a, b) if !(curCoordSetB intersect b).isEmpty => sideWalker(rest, a ++ curCoordSetA, b ++ curCoordSetB)
                //case (a,b) if !(curCoordSetA intersect a).isEmpty && !(curCoordSetA intersect b).isEmpty
                case _ => {
                    println("Side walk ERROR!!!")
                    (Set(), Set())
                }
            }

             */


        }
    }

    val (allSideA, allSideB) = sideWalker(updatedWalkedCells)

    println
    //println(allSideA)
    //println(allSideB)

    val sideACoords: Set[Coord] = allSideA -- walkedCoords
    val sideBCoords: Set[Coord] = allSideB -- walkedCoords

    println
    //println(sideACoords)
    //println(sideBCoords)

    @tailrec
    def spread(curCoordFront: Set[Coord], isOutside: Boolean = false, accuCoords: Set[Coord] = Set()): (Boolean, Set[Coord]) = {
        if(curCoordFront.isEmpty)
            (isOutside, accuCoords)
        else {
            val outOfBoundsFront: Set[Coord] = curCoordFront.filter(_.outOfBounds)

            val newIsOutside: Boolean = if(!isOutside) !outOfBoundsFront.isEmpty else isOutside

            val newFront = (curCoordFront -- outOfBoundsFront).flatMap(f => allDirections.map(d => d+f)) -- walkedCoords -- accuCoords -- curCoordFront

            spread(newFront, newIsOutside, accuCoords ++ curCoordFront -- outOfBoundsFront)
        }
    }

    val (outsideA, fullCoordsA) = spread(sideACoords)
    val (outsideB, fullCoordsB) = spread(sideBCoords)

    println
    println(outsideA)
    println(fullCoordsA.size)
    println(outsideB)
    println(fullCoordsB.size)


    //val updatedWalkedCells: Vector[Cell] = (walkedCells - startCell + Cell(actualSChar, startCoord, 0)).toVector.sortBy(_.step) ///

    //with side coords only:
    val withASideCoords: Set[Cell] = updatedWalkedCells.toSet ++ (sideACoords.filter(c => !(c.outOfBounds)).foldLeft(Set(): Set[Cell])((a, b) => {a + Cell('#', b, -1)}))
    val withBSideCoords: Set[Cell] = updatedWalkedCells.toSet ++ (sideBCoords.filter(c => !(c.outOfBounds)).foldLeft(Set(): Set[Cell])((a, b) => {a + Cell('*', b, -1)}))


    val a1 = getGridFromWalk(withASideCoords)
    println
    //println(s"With A side Coords:\n${a1.map(_.mkString).mkString("\n")}")

    val b1 = getGridFromWalk(withBSideCoords)
    println
    //println(s"With A side Coords:\n${b1.map(_.mkString).mkString("\n")}")

    //val ab1 = getGridFromWalk(withASideCoords ++ withBSideCoords)
    //println(s"With AB side Coords:\n${ab1.map(_.mkString).mkString("\n")}")

    println
    //println(fullCoordsA intersect fullCoordsB)



    // walkedCoords

    //def fff = walkedCells.toVector.sortBy(_.step)


    @tailrec
    def cellsForShoelace(remainingCells: Set[Cell], curCell: Cell, accu: Vector[Cell] = Vector()): Vector[Cell] =  {
        if(remainingCells.isEmpty) accu :+ curCell
        else {
            val curStep = curCell.step
            val curCellMoves = moves(curCell.mazeChar).map(m => curCell.coord + m)

            //val nextCell: Cell = remainingCells.find(r => r.step == curStep+1 && curCellMoves.contains(r.coord)).get
            //val nextCell: Cell = remainingCells.find(r => r.step == curStep+1 || r.step == curStep-1 || r.step == curStep && curCellMoves.contains(r.coord)).get
            //val nextCell: Cell = remainingCells.find(r => (r.step == curStep+1 || r.step == curStep-1 || r.step == curStep) && curCellMoves.contains(r.coord)).get
            //val nextCell: Cell = remainingCells.find(r => curCellMoves.contains(r.coord)).get
            val nextCell: Option[Cell] = remainingCells.find(r => curCellMoves.contains(r.coord) && !(accu contains r) )

            if(nextCell.isDefined)
                cellsForShoelace(remainingCells - curCell, nextCell.get, accu :+ curCell)
            else
                accu :+ curCell
        }
    }


    val cellsOrderedInCycle: Vector[Cell] = cellsForShoelace(updatedWalkedCells.toSet, newStartCell)
    val cellsOrderedInFullCycle: Vector[Cell] = cellsOrderedInCycle :+ newStartCell

    println(cellsOrderedInCycle)
    println(cellsOrderedInCycle.size)

    val (sideACorners: Set[CornerCoord], sideBCorners: Set[CornerCoord]) = sideWalker2(cellsOrderedInCycle)
    println(s"Side A corners: $sideACorners")
    println(s"Side B corners: $sideBCorners")

    val sideA_Coords: Set[Coord] = sideACorners.flatMap(a => a.surroundingCoords) -- walkedCoords
    val sideB_Coords: Set[Coord] = sideBCorners.flatMap(b => b.surroundingCoords) -- walkedCoords

    val (isOutSideA, sideAllCoordsA) = spread(sideA_Coords: Set[Coord])
    val (isOutSideB, sideAllCoordsB) = spread(sideB_Coords: Set[Coord])

    println
    println(s"is outside: $isOutSideA")
    println(sideAllCoordsA)
    println(sideAllCoordsA.size)

    println
    println(s"is outside: $isOutSideB")
    println(sideAllCoordsB)
    println(sideAllCoordsB.size)


    //val xxxxxx = area = 0.5 * abs(sum(
    //     x[i] * y[(i + 1) % n] - x[(i + 1) % n] * y[i]      for i in range(n)))

    /*
    def shoelace_formula(x, y):
        n = len(x)
        area = 0.5 * abs(sum(x[i] * y[(i + 1) % n] - x[(i + 1) % n] * y[i] for i in range(n)))
        return area

    # Example usage:
    x = [1, 2, 4, 4, 1]
    y = [1, 5, 5, 1, 1]

    area = shoelace_formula(x, y)
    print("Area:", area)
     */

    val coordsForShoelace: Vector[Coord] = cellsOrderedInCycle.map(_.coord)

    def shoelace(coords: Vector[Coord]): Int = {
        val n: Int = coords.size
        val area: Int = math.round(
            math.abs((0 to n-1).map(i => coords(i).x * coords((i+1) % n).y - coords((i+1) % n).x * coords(i).y).sum) * 0.5).toInt

        area
    }

    //val yyy = shoelace(coordsForShoelace)
    //println(s"Area: $yyy")


}


