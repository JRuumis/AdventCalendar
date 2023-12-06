import scala.annotation.tailrec

object Day6WaitForIt extends App {
    val inputRaw: String = scala.io.Source.fromFile("./Sources/2023/Day6WaitForIt.txt").mkString.replace("\r\n"," ")

    val timeDistanceRaw: (Vector[String], Vector[String]) = inputRaw match {
        case s"Time: $times Distance: $distances" =>
            (times.split(" ").map(_.trim).filter(_ != "").toVector, distances.split(" ").map(_.trim).filter(_ != "").toVector)
    }

    @tailrec
    def validHoldTimes(raceTime: Long, recordRaceDistance: Long, holdTime: Long = 1, accuValidHoldTimes: Long = 0): Long =  {
        if (holdTime >= raceTime) accuValidHoldTimes
        else
            validHoldTimes(raceTime, recordRaceDistance, holdTime + 1,
                if ((raceTime - holdTime) * holdTime > recordRaceDistance) accuValidHoldTimes + 1 else accuValidHoldTimes
            )
    }

    // Part One
    val timesDistances = timeDistanceRaw match {case(a,b) => a.map(_.toLong) zip b.map(_.toLong)}
    val beatRecordsProduct: Long = timesDistances.map{case(t,d) => validHoldTimes(t,d)}.product
    println(s"Ways to beat record, multiplied: $beatRecordsProduct")

    // Part Two
    val (time, distance) = timeDistanceRaw match {case(a,b) => (a.mkString.toLong, b.mkString.toLong)}
    val beatRecord = validHoldTimes(time,distance)
    println(s"Ways to bear record, single game: $beatRecord")
}