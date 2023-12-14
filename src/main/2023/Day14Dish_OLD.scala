import scala.annotation.tailrec

object Day14Dish_OLD extends App {

    //val inputGrid: Vector[Vector[Char]] = scala.io.Source.fromFile("./Sources/2023/Day14Dish_TEST2.txt").getLines().toVector.map(_.toVector)
    //val inputGrid: Vector[Vector[Char]] = scala.io.Source.fromFile("./Sources/2023/Day14Dish_TEST1.txt").getLines().toVector.map(_.toVector)
    val inputGrid: Vector[Vector[Char]] = scala.io.Source.fromFile("./Sources/2023/Day14Dish.txt").getLines().toVector.map(_.toVector)

    println(inputGrid.mkString("\n"))
    println()

    @tailrec
    def roller1(
                  row: Vector[Char],
                  accuEmpty: Vector[Char] = Vector(),
                  accuRow: Vector[Char] = Vector(),
                  rolls: Int = 0
              ): (Vector[Char], Int) = (row, accuEmpty) match
    {
        case ( Vector(), Vector() ) => (accuRow, rolls)
        case ( Vector(), empties ) => (accuRow ++ empties, rolls)
        case ( '.' +: rest, empties ) => roller1(rest, '.' +: empties, accuRow, rolls)
        case ( '#' +: rest, empties ) => roller1(rest, Vector(), accuRow ++ empties :+ '#', rolls)
        case ( 'O' +: rest, empties ) => roller1(rest, Vector(), (accuRow :+ 'O') ++ empties, rolls + empties.size)
        case _ => {
            println("match error!!!")
            (Vector(), 0)
        }
    }

    @tailrec
    def roller2(row: Vector[Char]): Vector[Char] = {
        val (newRow, rollsDone) = roller1(row)
        if(rollsDone > 0) roller2(newRow) else newRow
    }

    def gridRoller(grid: Vector[Vector[Char]]): Vector[Vector[Char]] = {
        grid.transpose.map(roller2(_)).transpose
    }


    @tailrec
    def rowWeightForReversed(row: Vector[Char], index: Int = 1, accu: Int = 0): Int = row match {
        case Vector() => accu
        case 'O' +: rest => rowWeightForReversed(rest, index + 1, accu + index)
        case _ +: rest => rowWeightForReversed(rest, index + 1, accu)
    }

    def rowWeight(row: Vector[Char]): Int = rowWeightForReversed(row.reverse)

    def gridWeightNorthSupport(grid: Vector[Vector[Char]]): Int = {
        grid.transpose.map(rowWeight(_)).sum
    }

    val gridRolled = gridRoller(inputGrid)

    val weight = gridWeightNorthSupport(gridRolled)
    println(weight)
    println


    def turnGrid(grid: Vector[Vector[Char]]): Vector[Vector[Char]] = {
        grid.transpose.map(_.reverse)
    }

    /*
    val g1 = turnGrid(inputGrid)
    println
    println(g1.mkString("\n"))

    val g2 = turnGrid(g1)
    println
    println(g2.mkString("\n"))

    val g3 = turnGrid(g2)
    println
    println(g3.mkString("\n"))

    val g4 = turnGrid(g3)
    println
    println(g4.mkString("\n"))

     */


    def doCycle(grid: Vector[Vector[Char]]): Vector[Vector[Char]] = {

        val g1Rolled = gridRoller(grid)
        //println(g1Rolled.mkString("\n") + "\n")

        val g1Turned = turnGrid(g1Rolled)
        //println(g1Turned.mkString("\n") + "\n")

        val g2Rolled = gridRoller(g1Turned)
        //println(g2Rolled.mkString("\n") + "\n")

        val g2Turned = turnGrid(g2Rolled)
        //println(g2Turned.mkString("\n") + "\n")

        val g3Rolled = gridRoller(g2Turned)
        //println(g3Rolled.mkString("\n") + "\n")

        val g3Turned = turnGrid(g3Rolled)
        //println(g3Turned.mkString("\n") + "\n")

        val g4Rolled = gridRoller(g3Turned)
        //println(g4Rolled.mkString("\n") + "\n")

        val g4Turned = turnGrid(g4Rolled)
        //println(g4Turned.mkString("\n") + "\n")

        g4Turned
    }

    /*
    println("--------")
    val after1Cycle = doCycle(inputGrid)
    val after2Cycle = doCycle(after1Cycle)
    val after3Cycle = doCycle(after2Cycle)
    val after4Cycle = doCycle(after3Cycle)

    println("after 1 cycle:")
    println(after1Cycle.mkString("\n"))

    println("after 2 cycle:")
    println(after2Cycle.mkString("\n"))

    println("after 3 cycle:")
    println(after3Cycle.mkString("\n"))

    println("after 4 cycle:")
    println(after4Cycle.mkString("\n"))

    */


    def cycler(
                  grid: Vector[Vector[Char]],
                  totalCycles: Int,
                  currentCycle: Int = 0,
                  accu: Map[Vector[Vector[Char]], Int] = Map()//,

              ): Vector[Vector[Char]] = {

        if(currentCycle == totalCycles) {
            grid
        } else {
            println(s"\ncycle: ${currentCycle} of ${totalCycles}")

            //val cyclesLeft: Int = totalCycles - currentCycle

            val loopFound: Boolean = accu contains grid

            val newMap: Map[Vector[Vector[Char]], Int] = if (loopFound) {
                println("LOOP!")
                println(s"${currentCycle} matches with ${accu(grid)}")
                accu
            } else {
                accu.updated(grid, currentCycle)
            }

            val loopLength: Option[Int] = if (loopFound)
                Some(currentCycle - accu(grid))
            else
                None

            val cyclesBeforeLoop: Option[Int] = if (loopFound) Some(currentCycle - loopLength.get - 1) else None

            val newTotalCycles: Int = if(loopFound)
                (totalCycles - cyclesBeforeLoop.get) % loopLength.get + cyclesBeforeLoop.get + loopLength.get
            else
                totalCycles

            println(s"New total cycles: ${newTotalCycles}")

            if(newTotalCycles == 0)
                grid
            else {
                val nextGrid = doCycle(grid)
                println("Next Grid:")
                println(nextGrid.mkString("\n"))

                cycler(nextGrid, newTotalCycles, currentCycle+1, newMap)
            }
        }
    }

    val xxxx = cycler(inputGrid, 1000000000)

    println
    println("----------")

    println(xxxx.mkString("\n"))
    println(gridWeightNorthSupport(xxxx))

}
