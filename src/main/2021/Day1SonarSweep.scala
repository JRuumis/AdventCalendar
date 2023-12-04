import scala.collection.+:

object Day1SonarSweep extends App {

    val depths: Vector[Int] = scala.io.Source.fromFile("./Sources/2021/Day1SonarSweep.txt").getLines().map(_.toInt).toVector

    // Part One
    def countIncreases(depths: Vector[Int], depthIncreases: Int = 0): Int = depths match {
        case a +: b +: rest if a < b => countIncreases(b +: rest, depthIncreases + 1)
        case _ +: b +: rest => countIncreases(b +: rest, depthIncreases)
        case _ => depthIncreases
    }

    val increases: Int = countIncreases(depths)

    println(s"Number of depth increases is $increases")

    // Part Two
    def countIncreasesWithWindow(depths: Vector[Int], depthIncreases: Int = 0): Int = depths match {
        case a +: b +: c +: d +: rest if a+b+c < b+c+d => countIncreasesWithWindow(b +: c +: d +: rest, depthIncreases + 1)
        case _ +: b +: c +: d +: rest => countIncreasesWithWindow(b +: c +: d +: rest, depthIncreases)
        case _ => depthIncreases
    }

    val increasesWithWindow: Int = countIncreasesWithWindow(depths)

    println(s"Number of depth increases with window is $increasesWithWindow")
}
