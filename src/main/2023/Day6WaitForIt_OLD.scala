import scala.annotation.tailrec

object Day6WaitForIt_OLD extends App {

    val inputsRaw = scala.io.Source.fromFile("./Sources/2023/Day6WaitForIt.txt").getLines().toVector

    println(inputsRaw)

    val linesParsed: Vector[Vector[Long]] = inputsRaw.map(_ match {
        case s"Time: $times" => times.split(" ").map(_.trim).filter(_ != "").map(_.toLong).toVector
        case s"Distance: $distances" => distances.split(" ").map(_.trim).filter(_ != "").map(_.toLong).toVector
        case _ => Vector()
    })

    val timesDistances: Vector[(Long, Long)] = linesParsed(0) zip linesParsed(1)

    println(timesDistances)

    val linesParsed2 = inputsRaw.map(_ match {
        case s"Time: $times" => times.replace(" ","").toLong
        case s"Distance: $distances" => distances.replace(" ","").toLong
        //case _ => Vector()
    })

    println(linesParsed2)

    @tailrec
    def counter(raceTime: Long, recordRaceDistance: Long, holdTime: Long = 1, accuValidHoldTimes: Long = 0): Long = holdTime match {
        case ht if ht >= raceTime => accuValidHoldTimes
        case ht => {
            val leftTime: Long = raceTime-holdTime
            val speed: Long = holdTime
            val distance: Long = leftTime * speed

            counter(raceTime, recordRaceDistance, holdTime+1, if(distance > recordRaceDistance) accuValidHoldTimes + 1 else accuValidHoldTimes)
        }
    }

    val yyy = timesDistances.map(_ match {
        case (time,distance) => counter(time,distance)
    })

    println(yyy)

    val zzz = yyy.product

    println(zzz)

    // linesParsed2
    val aaa = counter(linesParsed2(0), linesParsed2(1))

    println(aaa)

}
