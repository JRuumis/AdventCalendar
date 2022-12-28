object Day4CampCleanup extends App {

    val sectionAssignmentsSource: List[String] = scala.io.Source.fromFile("./Sources/Day4CampCleanup.txt").getLines().toList

    case class Interval(from: Int, to: Int) {
        def fullOverlap(other: Interval): Boolean = if (from >= other.from && to <= other.to || from <= other.from && to >= other.to) true else false

        def overlap(other: Interval): Boolean = if (from >= other.from && from <= other.to || to >= other.from && to <= other.to || (this fullOverlap other)) true else false
    }

    val sectionAssignments: List[List[Interval]] = sectionAssignmentsSource
        .map(a => a.split(",").toList.map(b => b.split("-").toList)
            .map { case (List(a: String, b: String)) => Interval(a.toInt, b.toInt) })

    val fullOverlapPairs = sectionAssignments.map { case (List(a: Interval, b: Interval)) => a fullOverlap b }.map(if (_) 1 else 0)
    val fullOverlapPairsSum = fullOverlapPairs.sum

    println(s"Number of full overlap pairs: ${fullOverlapPairsSum}")

    val overlapPairs = sectionAssignments.map { case (List(a: Interval, b: Interval)) => a overlap b }.map(if (_) 1 else 0)
    val overlapPairsSum = overlapPairs.sum

    println(s"Number of overlap pairs: ${overlapPairsSum}")
}
