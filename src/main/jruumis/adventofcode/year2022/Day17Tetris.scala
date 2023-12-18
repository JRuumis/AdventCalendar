package jruumis.adventofcode.year2022

object Day17Tetris extends App {

    val moveInput: Vector[Char] = scala.io.Source.fromFile("./Sources/Day17Tetris.txt").toVector

    case class Coord(y: Int, x: Int) {
        def +(other: Coord) = Coord(y + other.y, x + other.x)

        def neighbours: Vector[Coord] = Vector(this + Coord(0, 1), this + Coord(0, -1), this + Coord(1, 0), this + Coord(-1, 0)) // lazy val
    }

    case class Shape(shape: Vector[Vector[Char]], dx: Int, dy: Int) {
        val shapeCoord: Vector[Coord] = (0 to dy - 1).flatMap(y => (0 to dx - 1).map(x => if (shape(y)(x) == '@') Some(Coord(y, x)) else None)).filter(_.isDefined).map(_.get).toVector

        override def toString: String = shape.map(_.mkString).mkString("\n")
    }

    val shape1 = Shape(shape = Vector(Vector('@', '@', '@', '@')), dx = 4, dy = 1) // -
    val shape2 = Shape(shape = Vector(Vector('.', '@', '.'), Vector('@', '@', '@'), Vector('.', '@', '.')), dx = 3, dy = 3) // +
    val shape3 = Shape(shape = Vector(Vector('.', '.', '@'), Vector('.', '.', '@'), Vector('@', '@', '@')), dx = 3, dy = 3) // J
    val shape4 = Shape(shape = Vector(Vector('@'), Vector('@'), Vector('@'), Vector('@')), dx = 1, dy = 4) // I
    val shape5 = Shape(shape = Vector(Vector('@', '@'), Vector('@', '@')), dx = 2, dy = 2) // O

    val shapesOrdered: Vector[Shape] = Vector(shape1, shape2, shape3, shape4, shape5)

    case class Revolver[T](v: Vector[T], currentPosition: Int = 0) {
        val curVal = v(currentPosition)

        def nextTurn: Revolver[T] = Revolver(v, (currentPosition + 1) % v.length)

        def reset: Revolver[T] = Revolver(v, 0)
    }

    val moveRevolver: Revolver[Char] = Revolver(moveInput)
    val shapeRevolver: Revolver[Shape] = Revolver(shapesOrdered)
    val pushFallRevolver: Revolver[String] = Revolver(Vector("push", "fall"))

    val gridWidth = 7


    type Grid = Vector[Vector[Char]]

    def gridGet(grid: Grid, c: Coord): Option[Char] = c match {
        case Coord(y, x) if grid.length == 0 || y < 0 || y >= grid.length || x < 0 || x >= gridWidth => None
        case Coord(y, x) => Some(grid(y)(x))
    }

    def gridUpdateCell(grid: Grid, coord: Coord, newChar: Char): Grid = {
        grid.updated(coord.y, grid(coord.y).updated(coord.x, newChar))
    }

    def gridUpdateCells(grid: Grid, coords: Vector[Coord], newChar: Char): Grid = coords match {
        case Vector() => grid
        case coord +: rest => gridUpdateCells(
            gridUpdateCell(grid, coord, newChar),
            rest,
            newChar
        )
    }



    //    def updateCellInGrid(grid: Grid, shapeCoord: Vector[Coord], newChar: Char): Grid = {
    //        shapeCoord.foldLeft(grid)( (g,c) => g.updated(c.y, g(c.y).updated(c.x, newChar) ) )
    //    }


    case class Pit(grid: Grid, depth: Long, shapes: Revolver[Shape], moves: Revolver[Char], pushFall: Revolver[String], fallingShapeCornerCoord: Option[Coord]) {

        override def toString: String = "=======\n" + grid.map(_.mkString).mkString("\n") + "\n=======\n" + s"next shape:\n${shapes.curVal}\nnext move: ${moves.curVal}\nnext PushFall:${pushFall.curVal}\n"

        def addDepth(d: Long): Pit =
            Pit(grid, depth + d, shapes, moves, pushFall, fallingShapeCornerCoord)

        def addBlanksToTop(by: Int): Pit = {
            if (by <= 0) this
            else Pit(".......".toVector +: grid, depth + 1, shapes, moves, pushFall, fallingShapeCornerCoord).addBlanksToTop(by - 1)
        }

        def insertShape: Pit = {
            val curShape: Shape = shapes.curVal
            val dy: Int = curShape.dy

            val extendedPit: Pit = this.addBlanksToTop(3 + dy)
            val gridWithShapeAdded: Grid = gridUpdateCells(extendedPit.grid, curShape.shapeCoord.map(c => c + Coord(0, 2)), '@')

            Pit(gridWithShapeAdded, extendedPit.depth, extendedPit.shapes, extendedPit.moves, extendedPit.pushFall, Some(Coord(0, 2)))
        }

        def removeBlanksFromTop: Pit = {
            val emptyRow: Vector[Char] = ".......".toVector

            this.grid match {
                case `emptyRow` +: `emptyRow` +: rest => Pit(emptyRow +: rest, depth - 1, shapes, moves, pushFall, fallingShapeCornerCoord).removeBlanksFromTop
                case `emptyRow` +: rest => Pit(rest, depth - 1, shapes, moves, pushFall, fallingShapeCornerCoord)
                case _ => this
            }
        }

        def moveShapeInPit: Pit = {

            val newPit = this //             val newPit = this

            //println(newPit)

            val moveDelta: Coord = (newPit.moves.curVal, newPit.pushFall.curVal) match {
                case ('<', "push") => Coord(0, -1)
                case ('>', "push") => Coord(0, 1)
                case (_, "fall") => Coord(1, 0)
            }

            val fallingShapeCoords: Vector[Coord] = newPit.shapes.curVal.shapeCoord.map(c => c + fallingShapeCornerCoord.get)
            val fallingShapeCoordsAfterMove = fallingShapeCoords.map(c => c + moveDelta)

            val allowedAndSettle: Vector[(Boolean, Boolean)] = fallingShapeCoordsAfterMove.map(c => c match {
                case Coord(y, _) if y >= newPit.grid.length => (false, true)
                case Coord(_, x) if x < 0 || x >= 7 => (false, false)
                case Coord(y, x) if newPit.grid(y)(x) == '.' => (true, false)
                case Coord(y, x) if newPit.grid(y)(x) == '@' => (true, false)
                case Coord(y, x) if newPit.grid(y)(x) == '#' => (false, newPit.pushFall.curVal == "fall")
            })

            val allowedAndSettleSum = allowedAndSettle.reduce { (a, b) => (a._1 && b._1, a._2 || b._2) }

            allowedAndSettleSum match {
                case (true, false) => {
                    val newGridErased = gridUpdateCells(newPit.grid, fallingShapeCoords, '.')
                    val newGridMoved = gridUpdateCells(newGridErased, fallingShapeCoordsAfterMove, '@')

                    Pit(newGridMoved, newPit.depth, newPit.shapes, if (newPit.pushFall.curVal == "fall") newPit.moves.nextTurn else newPit.moves, newPit.pushFall.nextTurn, Some(newPit.fallingShapeCornerCoord.get + moveDelta)).moveShapeInPit
                }

                case (_, true) => {
                    val newGrid = gridUpdateCells(newPit.grid, fallingShapeCoords, '#')
                    Pit(newGrid, newPit.depth, newPit.shapes.nextTurn, newPit.moves.nextTurn, newPit.pushFall.reset, None)
                }

                case (false, false) => {
                    Pit(newPit.grid, newPit.depth, newPit.shapes, if (newPit.pushFall.curVal == "fall") newPit.moves.nextTurn else newPit.moves, newPit.pushFall.nextTurn, Some(newPit.fallingShapeCornerCoord.get)).moveShapeInPit
                }
            }
        }

        def dropNewShape: Pit = {
            val pitWithShape: Pit = this.insertShape
            pitWithShape.moveShapeInPit
        }


        def gridReach(inputGrid: Grid, currentFront: Set[Coord], accuRocksToKeep: Set[Coord]): Set[Coord] = {
            if (currentFront.isEmpty) accuRocksToKeep
            else {
                val neigboursToCheck: Set[Coord] = currentFront.flatMap(c => c.neighbours)

                val checkedNeighbours: Set[(Coord, Char)] = neigboursToCheck.map(c => (c, gridGet(inputGrid, c)) match {
                    case (_, None) => None
                    case (_, Some('f')) => None
                    case (_, Some('w')) => None
                    case (c, Some('.')) => Some((c, 'f'))
                    case (c, Some('#')) => Some((c, '#'))
                    //case (_, Some('*')) => None
                }).filter(_.isDefined).map(_.get)

                val newRocksToKeep: Set[Coord] = checkedNeighbours.filter { case (_, i) => i == '#' }.map { case (c, _) => c }
                val newFront: Set[Coord] = checkedNeighbours.filter { case (_, i) => i == 'f' }.map { case (c, _) => c }

                val gridNewFront: Grid = checkedNeighbours.foldLeft(inputGrid) { case (grid, (coord, char)) => gridUpdateCell(grid, coord, char) }
                val gridChangedFrontToWater: Grid = currentFront.foldLeft(gridNewFront) { case (grid, c) => gridUpdateCell(grid, c, 'w') }

                gridReach(gridChangedFrontToWater, newFront, accuRocksToKeep ++ newRocksToKeep)
            }
        }


        def removeInvisibleFromBottom: Pit = {

            val startFrontCoord: Coord = Coord(0, 0)
            val gridWithStartFrontSet: Grid = gridUpdateCell(this.grid, Coord(0, 0), 'f')

            val rocksToKeepCoords = gridReach(gridWithStartFrontSet, Set(startFrontCoord), Set())

            val maxYForNewGrid: Int = if (rocksToKeepCoords.isEmpty) 0 else rocksToKeepCoords.map(_.y).max
            val newGrid: Grid = (0 to maxYForNewGrid).toVector.map(y => (0 to gridWidth - 1).toVector.map(x => {
                if (rocksToKeepCoords contains Coord(y, x)) '#' else '.'
            }))

            Pit(newGrid, depth, shapes, moves, pushFall, fallingShapeCornerCoord)
        }

    }

    val startPit: Pit = Pit(Vector(), 0, shapeRevolver, moveRevolver, pushFallRevolver, None)

    //val moves: Long = 15
    //val maxMoves: Long = 2022
    val maxMoves: Long = 1000000000000l


    def iteratePit(currentPit: Pit, prevDepth: Long, move: Long, movesLeft: Long, stateAccu: Vector[(Grid, Int, Int, Long, Long)], stateMapForDetection: Map[(Grid, Int, Int), (Long, Long)]): Long = move match {
        case _ if movesLeft == 0 => currentPit.depth
        case _ => {

            val updatedPit: Pit = currentPit.addBlanksToTop(1).removeInvisibleFromBottom.removeBlanksFromTop

            val currentState = (updatedPit.grid, updatedPit.moves.currentPosition, updatedPit.shapes.currentPosition)
            val currentStateWithDepthAndStep = (updatedPit.grid, updatedPit.moves.currentPosition, updatedPit.shapes.currentPosition, move, updatedPit.depth)


            if (stateMapForDetection contains currentState) {

                // depth is depth before move

                val (cycleStartsAtMove, cycleStartsWithDepth) = stateMapForDetection(currentState)

                val lastCycleState = stateAccu.last
                val firstCycleState = stateAccu(cycleStartsAtMove.toInt - 1)

                val lastStateBeforeCycleStarts = stateAccu(cycleStartsAtMove.toInt - 2)

                val depthBeforeCycleStarts = lastStateBeforeCycleStarts._5
                val moveBeforeCycleStarts = lastStateBeforeCycleStarts._4

                val depthWhenCycleStarts = firstCycleState._5
                val moveWhenCycleStarts = firstCycleState._4

                val depthWhenCycleEnds = lastCycleState._5
                val moveWhenCycleEnds = lastCycleState._4

                //val cycleDepthDDD = depthWhenCycleEnds - depthWhenCycleStarts
                val cycleDepthDDD = depthWhenCycleEnds - depthBeforeCycleStarts
                val cycleMovesDDD = moveWhenCycleEnds - moveBeforeCycleStarts

                //val newMaxMovesXXX = (maxMoves - moveBeforeCycleStarts) % cycleMovesDDD + moveBeforeCycleStarts + cycleMovesDDD // add the cycle that we are already in !!! <-- not needed with the new approach

                val movesIntoCycle = (maxMoves - moveBeforeCycleStarts) % cycleMovesDDD
                val cyclesRequired = (maxMoves - moveBeforeCycleStarts) / cycleMovesDDD

                val depthWhenMovesIntoCycle = stateAccu(moveBeforeCycleStarts.toInt + movesIntoCycle.toInt /* - 1*/)._5 - depthBeforeCycleStarts //depthWhenCycleStarts


                val requiredMovesToGetTheSameResult = moveBeforeCycleStarts + movesIntoCycle

                val xxx = depthBeforeCycleStarts + cycleDepthDDD * cyclesRequired + depthWhenMovesIntoCycle


                println(xxx)
                xxx


                //val newMaxMoves = (maxMoves - moveBeforeCycleStarts) % cycleMovesDDD + moveBeforeCycleStarts


                //val xxx = depthBeforeCycleStarts + cycleDepthDDD
                //val ccc = updatedPit.depth - xxx


                /*


                //val cycleDepthDelta = prevDepth - cycleStartsWithDepth
                val cycleDepthDelta = currentPit.depth - cycleStartsWithDepth

                val cycleLength = move - cycleStartsAtMove

                //val newMaxMoves = ((move + movesLeft - (cycleStartsAtMove-1)) % cycleLength) + (cycleStartsAtMove-1) + cycleLength

                //val maxXX = maxMoves
                val newMaxMoves = (maxMoves - (cycleStartsAtMove-1)) % cycleLength + (cycleStartsAtMove-1) + (cycleLength-1)
                val updatedMovesLeft = newMaxMoves - move

                //val cycleDepth

                //val adjustedDepth = ((maxMoves - (cycleStartsAtMove-1)) / cycleLength) * cycleDepthDelta //- (updatedPit.depth - prevDepth)
                val adjustedDepth = ((maxMoves - (cycleStartsAtMove-1)) / cycleLength) * cycleDepthDelta - (updatedPit.depth - prevDepth)
                //val adjustedDepth = ((maxMoves - (cycleStartsAtMove-1)) / cycleLength) * cycleDepthDelta //- (prevDepth )

                println(updatedMovesLeft)



                iteratePit(updatedPit.addDepth(adjustedDepth).dropNewShape.removeBlanksFromTop, updatedPit.depth, move+1, updatedMovesLeft-1, Vector(), Map())
                //iteratePit(updatedPit.dropNewShape.removeBlanksFromTop, updatedPit.depth, move+1, updatedMovesLeft-1, Map())


 */
            } else {
                val ccc = updatedPit.dropNewShape.removeBlanksFromTop

                iteratePit(ccc, updatedPit.depth, move + 1, movesLeft - 1, stateAccu :+ currentStateWithDepthAndStep, (stateMapForDetection + (currentState -> (move, updatedPit.depth))))
            }

        }
    }


    val rrrrr: Long = iteratePit(startPit, 0, 1, maxMoves, Vector(), Map())
    println(rrrrr)



    //println("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
    //val truncatedPit: Pit = endPit.addBlanksToTop(1).removeInvisibleFromBottom.removeBlanksFromTop
    //println(truncatedPit)


    //println
    //println(endPit.grid.length)
    //println(endPit.depth)
}
