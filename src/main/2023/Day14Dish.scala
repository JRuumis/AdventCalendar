import scala.annotation.tailrec

object Day14Dish extends App {

    //val inputGridRaw: Vector[Vector[Char]] = scala.io.Source.fromFile("./Sources/2023/Day14Dish_TEST2.txt").getLines().toVector.map(_.toVector)
    //val inputGridRaw: Vector[Vector[Char]] = scala.io.Source.fromFile("./Sources/2023/Day14Dish_TEST1.txt").getLines().toVector.map(_.toVector)
    val inputGridRaw: Vector[Vector[Char]] = scala.io.Source.fromFile("./Sources/2023/Day14Dish.txt").getLines().toVector.map(_.toVector)

    case class Grid(grid: Vector[Vector[Char]]) {
        def roll: Grid = Grid(grid.transpose.map(Grid.rowRoller(_)).transpose)
        lazy val weight: Int = grid.transpose.map(Grid.rowWeight(_)).sum
        def turn: Grid = Grid(grid.transpose.map(_.reverse))
        def cycle: Grid = this.roll.turn.roll.turn.roll.turn.roll.turn
        def cycles(numberOfCycles: Int): Grid = Grid.cycler(this, numberOfCycles)

        override def toString: String = grid.map(_.mkString).mkString("\n")
    }
    object Grid {
        // rollers
        @tailrec
        private def rowRoller1(
                                  row: Vector[Char],
                                  accuEmpty: Vector[Char] = Vector(),
                                  accuRow: Vector[Char] = Vector(),
                                  rolls: Int = 0
                              ): (Vector[Char], Int) = (row, accuEmpty) match {
            case (Vector(), Vector()) => (accuRow, rolls)
            case (Vector(), empties) => (accuRow ++ empties, rolls)
            case ('.' +: rest, empties) => rowRoller1(rest, '.' +: empties, accuRow, rolls)
            case ('#' +: rest, empties) => rowRoller1(rest, Vector(), accuRow ++ empties :+ '#', rolls)
            case ('O' +: rest, empties) => rowRoller1(rest, Vector(), (accuRow :+ 'O') ++ empties, rolls + empties.size)
            case _ => {
                println("match error!!!")
                (Vector(), 0)
            }
        }

        @tailrec
        private def rowRoller(row: Vector[Char]): Vector[Char] = {
            val (newRow, rollsDone) = rowRoller1(row)
            if (rollsDone > 0) rowRoller(newRow) else newRow
        }

        // weighters
        @tailrec
        private def rowWeightForReversed(row: Vector[Char], index: Int = 1, accu: Int = 0): Int = row match {
            case Vector() => accu
            case 'O' +: rest => rowWeightForReversed(rest, index + 1, accu + index)
            case _ +: rest => rowWeightForReversed(rest, index + 1, accu)
        }

        private def rowWeight(row: Vector[Char]): Int = rowWeightForReversed(row.reverse)

        @tailrec
        private def cycler(
                              inputGrid: Grid,
                              totalCycles: Int,
                              currentCycle: Int = 0,
                              loopSearcher: Map[Grid, Int] = Map()
                          ): Grid = {

            if (currentCycle == totalCycles) {
                inputGrid
            } else {
                val loopFound: Boolean = loopSearcher contains inputGrid

                val updatedLoopSearcher: Map[Grid, Int] = if (loopFound)
                    loopSearcher
                else
                    loopSearcher.updated(inputGrid, currentCycle)

                val loopLength: Option[Int] = if (loopFound) {
                    //println(s"\tLoop found! Cycle ${currentCycle} matches with Cycle ${loopSearcher(inputGrid)}. Loop length: ${currentCycle - loopSearcher(inputGrid)}")
                    Some(currentCycle - loopSearcher(inputGrid))
                } else
                    None

                val cyclesBeforeLoop: Option[Int] = if (loopFound) Some(currentCycle - loopLength.get - 1) else None

                val newTotalCycles: Int = if (loopFound) {
                    (totalCycles - cyclesBeforeLoop.get) % loopLength.get + cyclesBeforeLoop.get + loopLength.get
                } else
                    totalCycles

                if (newTotalCycles == currentCycle)
                    inputGrid
                else {
                    val nextGrid = inputGrid.cycle
                    cycler(nextGrid, newTotalCycles, currentCycle + 1, updatedLoopSearcher)
                }
            }
        }
    }

    val startGrid: Grid = Grid(inputGridRaw)

    // Part One
    val gridRolled: Grid = startGrid.roll
    println(s"Rolled grid weight: ${gridRolled.weight}")

    // Part Two
    val cycledGrid: Grid = startGrid.cycles(1000000000)
    println(s"Cycled grid weight: ${cycledGrid.weight}")
}