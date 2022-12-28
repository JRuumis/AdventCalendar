import scala.annotation.tailrec
import scala.math.abs
import scala.util.matching.Regex

object Day15Beacons extends App {

    type DimCoord = Long

    //val checkY: DimCoord = 10 // TEST
    //val searchSpaceMax: Long = 20 // TEST
    val checkLineY: DimCoord = 2000000
    val searchSpaceMax: Long = 4000000

    case class Coord(x: DimCoord, y: DimCoord) {
        def manhattanDistance(other: Coord): Long = abs(x - other.x) + abs(y - other.y)
    }

    val sensorBeaconInputRaw: Vector[String] = scala.io.Source.fromFile("./Sources/Day15Beacons.txt").getLines.toVector
    val sensorBeaconPattern: Regex = "Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)".r
    val sensorNearestBeaconPairs: Vector[(Coord, Coord)] = sensorBeaconInputRaw.map(a => a match {
        case sensorBeaconPattern(sx, sy, bx, by) => (Coord(sx.toLong, sy.toLong), Coord(bx.toLong, by.toLong))
    })

    // -= Part 1 =-
    println("\nStarting Part 1 search...")
    val allSensorCoords: Set[Coord] = sensorNearestBeaconPairs.map { case (s, _) => s }.toSet
    val allBeaconCoords: Set[Coord] = sensorNearestBeaconPairs.map { case (_, b) => b }.toSet

    def allCoordsOnLineYThatCannotContainBeacon(sensorX: Long, sensorY: Long, distanceToNearestBeacon: Long): Set[Coord] = {
        val distanceToY: Long = abs(checkLineY - sensorY)

        if (distanceToY > distanceToNearestBeacon) Set()
        else {
            val distanceX = distanceToNearestBeacon - distanceToY
            (sensorX - distanceX to sensorX + distanceX).map(xx => Coord(xx, checkLineY)).toSet // returning discrete coords here, this will not work in Part 2
        }
    }

    val coordinatesCannotContainBeacon: Set[Coord] = sensorNearestBeaconPairs.flatMap { case (sensor, beacon) =>
        (allCoordsOnLineYThatCannotContainBeacon(sensor.x, sensor.y, sensor.manhattanDistance(beacon)) -- allSensorCoords -- allBeaconCoords)
    }.toSet

    println(s"Nr of positions on line ${checkLineY} that cannot contain a Beacon: ${coordinatesCannotContainBeacon.size}")


    // -= Part 2 =-
    println("\nStarting Part 2 search...")

    val sensorsAndDistances: Vector[(Coord, Long)] = sensorNearestBeaconPairs.map { case (s, b) => (s, s.manhattanDistance(b)) }

    def normaliseCoord(c: DimCoord): DimCoord = c match {
        case i if i < 0 => 0
        case i if i > searchSpaceMax => searchSpaceMax
        case i => i
    }

    @tailrec
    def mergeIntervals(inputIntervals: Vector[(Long, Long)], outputIntervals: Vector[(Long, Long)] = Vector()): Vector[(Long, Long)] = inputIntervals match {
        // the intervals are assumed to already been ordered by first element ascending!!!
        case (a1, a2) +: (b1, b2) +: rest if b2 < a2 => mergeIntervals((a1, a2) +: rest, outputIntervals) // a1 >> b1 >> b2 >> a2 --> (b1,b2) is eliminated
        case (a1, a2) +: (b1, b2) +: rest if a2 >= b1 => mergeIntervals((a1, b2) +: rest, outputIntervals) // a1 >> b1 >> a2 >> b2 --> intervals overlap
        case (a1, a2) +: (b1, b2) +: rest if a2 < b1 => mergeIntervals((b1, b2) +: rest, outputIntervals :+ (a1, a2)) // a1 >> a2 >> b1 >> b2 --> intervals do not overlap
        case (a1, a2) +: Vector() => outputIntervals :+ (a1, a2)
    }

    @tailrec
    def walkY(currentY: Long = 0): Option[Coord] = currentY match {
        case curY if curY > searchSpaceMax => None
        case curY => {
            val currentBetweens: Vector[(Long, Long)] = sensorsAndDistances.map { case ((Coord(sensorX, sensorY), sensorD)) => {
                val curDistFromSensorY = abs(sensorY - curY)

                if (curDistFromSensorY > sensorD) None
                else Some(normaliseCoord(sensorX - (sensorD - curDistFromSensorY)), normaliseCoord(sensorX + (sensorD - curDistFromSensorY)))
            }
            }.filter(_.isDefined).map(_.get).sortBy { case (a, _) => a }

            val currentMergedBetweens = mergeIntervals(currentBetweens)

            if (currentMergedBetweens.size > 1) {
                Some(Coord(currentMergedBetweens(0)._2 + 1, curY))
            } else {
                walkY(curY + 1)
            }
        }
    }

    val distressCoord: Coord = walkY().get
    val distressTuningFrequency: Long = distressCoord.x * 4000000 + distressCoord.y

    print(s"The missing Beacon can be reached at Tuning Frequency: ${distressTuningFrequency}")
}
