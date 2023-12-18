package jruumis.adventofcode.year2022

import scala.annotation.tailrec
import scala.math.abs
import scala.util.matching.Regex

object Day15Beacons_2 extends App {


    val sensorBeaconInputRaw: Vector[String] = scala.io.Source.fromFile("./Sources/Day15Beacons.txt").getLines.toVector
    val sensorBeaconPattern: Regex = "Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)".r

    type DimCoord = Long

    val checkY: DimCoord = 2000000
    //val checkY: DimCoord = 10


    case class Coord(x: DimCoord, y: DimCoord) {
        def manhattanDistance(other: Coord): Long = {
            abs(x - other.x) + abs(y - other.y)
        }

        //val ggggg: Long = 10
        //val xxxx = (0 to ggggg-1)

        //        private def allCoordsAtDistance(distance: Long): Set[Coord] = {
        //            (0 to distance-1).toSet.flatMap(i => Vector( Coord(x-distance+i, y-i), Coord(x+i, y-distance+i), Coord(x+distance-i, y+i), Coord(x-i, y+distance-i)))
        //        }

        //        def allCoordsWithinDistance(distance: Long): Set[Coord] = {
        //            (1 to distance).toSet.flatMap(allCoordsAtDistance(_))
        //        }

        def allCoordsOnLineY(distance: Long): Set[Coord] = {
            val distanceToY: Long = abs(checkY - y)

            if (distanceToY > distance) Set() else {
                val distanceX = distance - distanceToY

                (x - distanceX to x + distanceX).map(xx => Coord(xx, checkY)).toSet
            }
        }
    }

    abstract class GridCitizen {
        val coord: Coord
    }

    case class Sensor(coord: Coord) extends GridCitizen

    case class Beacon(coord: Coord) extends GridCitizen

    val sensorNearestBeaconPairs = sensorBeaconInputRaw.map(a => a match {
        case (sensorBeaconPattern(sx, sy, bx, by)) => (Sensor(Coord(sx.toLong, sy.toLong)), Beacon(Coord(bx.toLong, by.toLong)))
    })

    //println(sensorNearestBeaconPairs.mkString("\n"))

    val allSensorCoords = sensorNearestBeaconPairs.map { case (s, _) => s.coord }.toSet
    val allBeaconCoords = sensorNearestBeaconPairs.map { case (_, b) => b.coord }.toSet

    val sensorsBeaconsDistancesCheckY = sensorNearestBeaconPairs.map { case (s, b) => (s, b, s.coord.allCoordsOnLineY(s.coord.manhattanDistance(b.coord)) -- allSensorCoords -- allBeaconCoords) }

    //println(sensorsBeaconsDistances.mkString("\n"))

    val xxx: Set[Coord] = sensorsBeaconsDistancesCheckY.flatMap { case (_, _, d) => d }.toSet //.filter { _.y == 2000000 /*10*/ }

    println("Part1: ")
    //println(xxx.toVector.sortBy {case(Coord(x,y)) => x})
    println(xxx.size)


    def pri(s: Sensor, b: Beacon, surroundings: Set[Coord]) = {
        val minX = (surroundings.map(_.x) + s.coord.x + b.coord.x).min
        val maxX = (surroundings.map(_.x) + s.coord.x + b.coord.x).max
        val minY = (surroundings.map(_.y) + s.coord.y + b.coord.y).min
        val maxY = (surroundings.map(_.y) + s.coord.y + b.coord.y).max

        (minY to maxY).map(yy => (minX to maxX).map(xx => (xx, yy) match {
            case (x, y) if surroundings contains Coord(x, y) => '#'
            case (x, y) if Coord(x, y) == s.coord => 'S'
            case (x, y) if Coord(x, y) == b.coord => 'B'
            case _ => '.'
        }).mkString).mkString("\n")
    }

    //val fff = pri(sensorsBeaconsDistances(0)._1, sensorsBeaconsDistances(0)._2, sensorsBeaconsDistances(0)._3)
    //println(fff)


    // Part 2
    //val searchSpaceMax = 20
    val searchSpaceMax: Long = 4000000

    //val sensorsBeaconsDistances = sensorNearestBeaconPairs.map{case(s,b) => (s,b, s.coord.allCoordsWithinDistance(s.coord.manhattanDistance(b.coord)) ++ allSensorCoords ++ allBeaconCoords  )}
    //val xxxx: Set[Coord] = sensorsBeaconsDistances.flatMap{case(_,_,d) => d}.toSet

    //val ggg: Set[Coord] = (0 to 20).flatMap(yy => (0 to 20).map(xx => Coord(xx,yy))).toSet

    //val h = ggg -- xxxx

    //println(h)


    val sensorsAndDistances: Vector[(Coord, Long)] = sensorNearestBeaconPairs.map { case (s, b) => (s.coord, s.coord.manhattanDistance(b.coord)) }

    @tailrec
    def walkX(curX: Long, currentBetweens: Vector[(Long, Long)]): Option[Long] = curX match {
        case x if x > searchSpaceMax => None
        case x => if (currentBetweens.find { case (from, to) => x >= from && x <= to }.isEmpty) Some(x) else walkX(curX + 1, currentBetweens)
    }

    def normSpace(i: Long): Long = i match {
        case i if i < 0 => 0
        case i if i > searchSpaceMax => searchSpaceMax
        case i => i
    }

    @tailrec
    def collapseVec(v: Vector[(Long, Long)], accu: Vector[(Long, Long)] = Vector()): Vector[(Long, Long)] = v match {
        case (a1, a2) +: (b1, b2) +: rest if a2 >= b1 && b2 < a2 => collapseVec((a1, a2) +: rest, accu)
        case (a1, a2) +: (b1, b2) +: rest if a2 >= b1 => collapseVec((a1, b2) +: rest, accu)
        case (a1, a2) +: (b1, b2) +: rest if a2 < b1 => collapseVec((b1, b2) +: rest, accu :+ (a1, a2))
        case (a1, a2) +: Vector() => accu :+ (a1, a2)
    }

    @tailrec
    def walkY(curY: Long = 0): Option[(Long, Long)] = curY match {
        case y if y > searchSpaceMax => None
        case y => {

            //println(s"y = ${y}")

            if (y % 100000 == 0) {
                print(".")
            } else {}

            val currentBetweens: Vector[(Long, Long)] = sensorsAndDistances.map { case ((Coord(xx, yy), dd)) => {
                val distanceY = abs(yy - y)

                if (distanceY > dd) None
                else Some(normSpace(xx - (dd - distanceY)), normSpace(xx + (dd - distanceY)))
            }
            }.filter(_.isDefined).map(_.get).sortBy { case (a, _) => a }

            //println(s"current unmerged X: ${currentBetweens}")

            val currentMergedBetweens = collapseVec(currentBetweens)

            //println(s"merged X: ${currentMergedBetweens}")

            if (currentMergedBetweens.size > 1) {
                Some(currentMergedBetweens(0)._2 + 1, y)
            } else {
                walkY(y + 1)
            }

            /*
            if(currentMergedBetweens == Vector((0,searchSpaceMax)))
                walkY(y+1)
            else {
                val currentFoundX: Option[Int] = walkX(0, currentBetweens)
                if(currentFoundX.isDefined) Some(currentFoundX.get, y) else walkY(y+1)
            }

             */

        }
    }

    println("searching missing sensor...")

    val gggg = walkY()
    println(gggg)

    val xxxx = gggg.get._1
    val yyy = gggg.get._2

    println(xxxx)
    println(yyy)

    val sss: Long = xxxx * 4000000 + yyy

    print(s"--->${sss}")


}
