import scala.util.matching.Regex

object Day14Reindeer extends App {

    val reindeersRaw: Vector[String] = scala.io.Source.fromFile("./Sources/2015/Day14Reindeer.txt").getLines().toVector
    val reindeerPattern: Regex = """([A-Za-z]+) can fly ([0-9]+) km/s for ([0-9]+) seconds, but then must rest for ([0-9]+) seconds\.""".r

    type ReindeerName = String
    type Speed = Int
    type Duration = Int
    type Distance = Int
    case class Reindeer(reindeer: ReindeerName, flySpeed: Speed, flyDuration: Duration, restDuration: Duration) {
        val cycleDuration = flyDuration + restDuration
        val cycleDistance = flySpeed * flyDuration

        def getDistance(duration: Duration): Distance = {
            val fullCycles: Int = duration / cycleDuration
            val secondsInCycle = duration % cycleDuration
            val flySecondsInCycle = if(secondsInCycle > flyDuration) flyDuration else secondsInCycle

            fullCycles * cycleDistance + flySecondsInCycle * flySpeed
        }
    }

    val reindeers: Vector[Reindeer] = reindeersRaw.map(r => r match {
        case reindeerPattern(a,b,c,d) => Reindeer(a,b.toInt,c.toInt,d.toInt)
    })

    val maxDuration: Duration = 2503

    val reindeerWinningDistance: Distance = reindeers.map(_.getDistance(maxDuration)).max

    println(s"Part 1: Winning reindeer has travelled ${reindeerWinningDistance} km.")


    val partTwoTracer = (1 to maxDuration).toVector
        .map(t => (t, reindeers.map(r => (r.reindeer -> r.getDistance(t))).toMap ))
        .map{case(t,distances) => (t,distances,distances.values.max)}
        .map{case(t,distances,max) => (t,distances,max,distances.map{case(r,d) => (r -> (if(d == max) 1 else 0 ))})}

    val scores = partTwoTracer.map{case(_,_,_,scores) => scores}
    val scoreTotals = scores.head.keySet.map(k => (k -> scores.map(_(k)).sum)).toMap

    println(s"Part 2: Winning reindeer gets ${scoreTotals.values.max} score.")
}