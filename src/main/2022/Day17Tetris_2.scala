object Day17Tetris_2 extends App {

    val moveInput: Vector[Char] = scala.io.Source.fromFile("./Sources/Day17Tetris_TEST.txt").toVector

    //println(moveInput)
    case class Coord(y: Int, x: Int) {
        def +(other: Coord) = Coord(y + other.y, x + other.x)
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

    case class Turner[T](v: Vector[T], currentPosition: Int = 0) {
        val curVal = v(currentPosition)

        def nextTurn: Turner[T] = Turner(v, (currentPosition + 1) % v.length)

        def reset: Turner[T] = Turner(v, 0)
    }

    val moveTurner: Turner[Char] = Turner(moveInput)
    val shapeTurner: Turner[Shape] = Turner(shapesOrdered)
    val pushFallTurner: Turner[String] = Turner(Vector("push", "fall"))

    //println(moveTurner)
    //println(shapeTurner)

    def updateGrid(grid: Vector[Vector[Char]], shapeCoord: Vector[Coord], newChar: Char): Vector[Vector[Char]] = {
        shapeCoord.foldLeft(grid)((g, c) => g.updated(c.y, g(c.y).updated(c.x, newChar)))
    }

    case class Pit(grid: Vector[Vector[Char]], depth: Int, shapes: Turner[Shape], moves: Turner[Char], pushFall: Turner[String], fallingShapeCornerCoord: Option[Coord]) {

        override def toString: String = "=======\n" + grid.map(_.mkString).mkString("\n") + "\n=======\n" + s"next shape:\n${shapes.curVal}\nnext move: ${moves.curVal}\nnext PushFall:${pushFall.curVal}\n"

        def extendPit(by: Int): Pit = {
            if (by <= 0) this
            else Pit(".......".toVector +: grid, depth + 1, shapes, moves, pushFall, fallingShapeCornerCoord).extendPit(by - 1)
        }

        def insertShape: Pit = {
            val curShape: Shape = shapes.curVal
            val dy: Int = curShape.dy

            val extendedPit = this.extendPit(3 + dy) // not always 3

            val newGrid = updateGrid(extendedPit.grid, curShape.shapeCoord.map(c => c + Coord(0, 2)), '@') //curShape.shapeCoord.foldLeft(extendedPit.grid)( (g,c) => g.updated(c.y, g(c.y).updated(c.x+2, '@') ) )

            Pit(newGrid, extendedPit.depth, extendedPit.shapes, extendedPit.moves, extendedPit.pushFall, Some(Coord(0, 2)))
        }

        def reducePitGridFromTop: Pit = {
            val emptyRow: Vector[Char] = ".......".toVector

            this.grid match {
                case `emptyRow` +: `emptyRow` +: rest => Pit(emptyRow +: rest, depth - 1, shapes, moves, pushFall, fallingShapeCornerCoord).reducePitGridFromTop
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
                    val newGridErased = updateGrid(newPit.grid, fallingShapeCoords, '.')
                    val newGridMoved = updateGrid(newGridErased, fallingShapeCoordsAfterMove, '@')

                    Pit(newGridMoved, newPit.depth, newPit.shapes, if (newPit.pushFall.curVal == "fall") newPit.moves.nextTurn else newPit.moves, newPit.pushFall.nextTurn, Some(newPit.fallingShapeCornerCoord.get + moveDelta)).moveShapeInPit
                }

                case (_, true) => {
                    val newGrid = updateGrid(newPit.grid, fallingShapeCoords, '#')
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
    }

    val startPit: Pit = Pit(Vector(), 0, shapeTurner, moveTurner, pushFallTurner, None)

    //val moves: Long = 2022
    val moves: Long = 1000000000000l

    def iteratePit(currentPit: Pit, movesLeft: Long): Pit = movesLeft match {
        case 0 => currentPit
        case _ => {
            if ((moves - movesLeft) % 1000000 == 0) {
                print("*")
            } else {}

            iteratePit(currentPit.reducePitGridFromTop.dropNewShape, movesLeft - 1)
        }
    }

    val endPit = iteratePit(startPit, moves)
    val endPitReduced = endPit.reducePitGridFromTop
    println(endPitReduced)


    println
    println(endPitReduced.grid.length)
    println(endPitReduced.depth)
}
