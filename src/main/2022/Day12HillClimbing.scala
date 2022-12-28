import scala.annotation.tailrec

object Day12HillClimbing extends App {

    val startGrid = scala.io.Source.fromFile("./Sources/Day12HillClimbing.txt").getLines().toVector.map(_.toVector)

    val sizeY = startGrid.length
    val sizeX = startGrid(0).length

    val startPosition: (Int, Int) = (0 to sizeY - 1).flatMap(y => (0 to sizeX - 1).map(x => if (startGrid(y)(x) == 'S') Some(y, x) else None)).filter(_.isDefined).map(_.get)(0)
    val finishPosition: (Int, Int) = (0 to sizeY - 1).flatMap(y => (0 to sizeX - 1).map(x => if (startGrid(y)(x) == 'E') Some(y, x) else None)).filter(_.isDefined).map(_.get)(0)

    def update[A](vec: Vector[Vector[A]], coord: (Int, Int), value: A): Vector[Vector[A]] =
        vec.updated(coord._1, vec(coord._1).updated(coord._2, value))

    val grid: Vector[Vector[Char]] = update(update(startGrid, startPosition, 'a'), finishPosition, 'z') // remove S and E from grid

    def nextStep(accuClimbGrid: Vector[Vector[Option[Int]]]): Vector[Vector[Option[Int]]] = {
        (0 to sizeY - 1).map(y => (0 to sizeX - 1).map { x =>
            accuClimbGrid(y)(x) match {
                case Some(a) => Some(a)
                case None => {
                    val v = Vector(
                        (if (y - 1 >= 0 && grid(y - 1)(x).toInt + 1 >= grid(y)(x).toInt && accuClimbGrid(y - 1)(x).isDefined) Some(accuClimbGrid(y - 1)(x).get) else None),
                        (if (y + 1 < sizeY && grid(y + 1)(x).toInt + 1 >= grid(y)(x).toInt && accuClimbGrid(y + 1)(x).isDefined) Some(accuClimbGrid(y + 1)(x).get) else None),
                        (if (x - 1 >= 0 && grid(y)(x - 1).toInt + 1 >= grid(y)(x).toInt && accuClimbGrid(y)(x - 1).isDefined) Some(accuClimbGrid(y)(x - 1).get) else None),
                        (if (x + 1 < sizeX && grid(y)(x + 1).toInt + 1 >= grid(y)(x).toInt && accuClimbGrid(y)(x + 1).isDefined) Some(accuClimbGrid(y)(x + 1).get) else None),
                    ).filter(_.isDefined)

                    if (v.isEmpty) None else Some(v.map(_.get).min + 1) // take lowest available number_of_steps + 1 from neighbours
                }
            }
        }.toVector).toVector
    }

    @tailrec
    def climb(accuClimbGrid: Vector[Vector[Option[Int]]]): Vector[Vector[Option[Int]]] = {
        if (accuClimbGrid(finishPosition._1)(finishPosition._2).isDefined) accuClimbGrid
        else climb(nextStep(accuClimbGrid))
    }

    // Part 1
    val climbGridPart1: Vector[Vector[Option[Int]]] = startGrid.map(y => y.map(x => if (x == 'S') Some(0) else None))
    val climbedGridPart1: Vector[Vector[Option[Int]]] = climb(climbGridPart1)
    val shortestPathPart1 = climbedGridPart1(finishPosition._1)(finishPosition._2).get

    println(s"Shortest path from Start to Finish is: ${shortestPathPart1}")

    // Part 2
    val climbGridPart2: Vector[Vector[Option[Int]]] = startGrid.map(y => y.map(x => if (x == 'S' || x == 'a') Some(0) else None))
    val climbedGridPart2 = climb(climbGridPart2)
    val shortestPathPart2 = climbedGridPart2(finishPosition._1)(finishPosition._2).get

    println(s"Shortest path from any lowest point to Finish is: ${shortestPathPart2}")
}
