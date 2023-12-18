package jruumis.adventofcode.year2023

import scala.annotation.tailrec

object Day17Clumsy_unrefactored extends App {

    //val inputGrid = scala.io.Source.fromFile("./Sources/2023/day17Clumsy_TEST2.txt").getLines().map(_.toVector.map(_.asDigit)).toVector
    //val inputGrid = scala.io.Source.fromFile("./Sources/2023/day17Clumsy_TEST1.txt").getLines().map(_.toVector.map(_.asDigit)).toVector
    val inputGrid = scala.io.Source.fromFile("./Sources/2023/day17Clumsy.txt").getLines().map(_.toVector.map(_.asDigit)).toVector

    println(inputGrid.mkString("\n"))

    val minGrid = 0
    val maxGrid = inputGrid.size-1

    val startCoord: Coord = Coord(minGrid, minGrid)
    val finishCoord: Coord = Coord(maxGrid, maxGrid)

    case class Coord(x: Int, y: Int) {
        def +(other: Coord) = Coord(x + other.x, y + other.y)
        lazy val isInGrid: Boolean = x >= minGrid && x <= maxGrid && y >= minGrid && y <= maxGrid
        lazy val distanceToFinish: Int = (finishCoord.x - x).abs + (finishCoord.y - y).abs
    }

    sealed trait Direction {
        val coord: Coord
        val opposite: Direction
        val allWalkingDirections: Set[Direction] = Set(Up, Down, Left, Right)
        lazy val nextDirections: Set[Direction] = allWalkingDirections - opposite
    }

    case object Up extends Direction {
        override val coord = Coord(0, -1)
        override val opposite: Direction = Down
    }

    case object Down extends Direction {
        override val coord = Coord(0, 1)
        override val opposite: Direction = Up
    }

    case object Left extends Direction {
        override val coord = Coord(-1, 0)
        override val opposite: Direction = Right
    }

    case object Right extends Direction {
        override val coord = Coord(1, 0)
        override val opposite: Direction = Left
    }

    case object Empty extends Direction {
        override val coord = Coord(0,0)
        override val opposite: Direction = Empty
        override lazy val nextDirections: Set[Direction] = allWalkingDirections
    }



    case class Position(coord: Coord, lastDirection: Direction, lastDirectionMulti: Int, accuHeatLoss: Int) {
        lazy val pos: (Coord, Direction, Int) = (coord, lastDirection, lastDirectionMulti)

        def move(newDirection: Direction): Option[Position] = {
            val newCoord: Coord = coord + newDirection.coord

            if(newCoord.isInGrid)
                Some(Position(newCoord, newDirection, if(newDirection == lastDirection) lastDirectionMulti+1 else 1, accuHeatLoss + inputGrid(newCoord.y)(newCoord.x)))
            else
                None
        }

        lazy val getMoves2: Vector[Position] = {
            if(lastDirectionMulti < 4 && lastDirection != Empty) {
                Vector(move(lastDirection)).filter(_.isDefined).map(_.get)
            } else if (lastDirectionMulti > 10 && lastDirection != Empty) {
               Vector()
            } else {
                lastDirection.nextDirections.map(nextDirect => move(nextDirect)).filter(_.isDefined).map(_.get).toVector
            }
        }

    }

    val startPosition = Position(startCoord, Empty, 0, 0)

    // cachers
    var bestHeatLossForPosition: scala.collection.mutable.HashMap[(Coord, Direction, Int), Int] = scala.collection.mutable.HashMap()
    //val altCache: scala.collection.mutable.HashMap[Coord, Int] = scala.collection.mutable.HashMap()


    @tailrec
    def walker2(startFront: Vector[Position], bestOverallHeatLossFound: Int): Int = startFront match {
        case Vector() => bestOverallHeatLossFound
        case positions => {

            val bestPosition: Position = positions.minBy { pos => pos.accuHeatLoss + pos.coord.distanceToFinish }

            val rest = positions.filter(_ != bestPosition)

            // PART ONE !!!!!!!!!!!!!!!!
            //val nextPositionCandidates: Vector[Position] = bestPosition.lastDirection.nextDirections.map(nextDirect => bestPosition.move(nextDirect)).filter(_.isDefined).map(_.get).toVector

            val nextPositionCandidates: Vector[Position] = bestPosition.getMoves2



            val nextPositionsForBestPosition: Vector[Position] = nextPositionCandidates
                //.filter(_.lastDirectionMulti <= 3) // PART TWO!!!!!!!!!
                .filter(pos => pos.accuHeatLoss + pos.coord.distanceToFinish < bestOverallHeatLossFound)
                .filter(pos => {
                    val bestCachedHeatLossForPos: Option[Int] = bestHeatLossForPosition.get((pos.coord, pos.lastDirection, pos.lastDirectionMulti))

                    if (bestCachedHeatLossForPos.isDefined && bestCachedHeatLossForPos.get <= pos.accuHeatLoss)
                        false
                    else
                        true
                })

            // cacher update
            nextPositionsForBestPosition.map(pos => {
                val bestCachedHeatLossForPos: Option[Int] = bestHeatLossForPosition.get((pos.coord, pos.lastDirection, pos.lastDirectionMulti))

                if (bestCachedHeatLossForPos.isEmpty || bestCachedHeatLossForPos.isDefined && bestCachedHeatLossForPos.get > pos.accuHeatLoss) {
                    bestHeatLossForPosition.update( (pos.coord, pos.lastDirection, pos.lastDirectionMulti), pos.accuHeatLoss)
                }
            })


            // min heat loss
            //val minHeatLossInFront: Option[Int] = nextPositionsForBestPosition.filter(_.coord == finishCoord).map(_.accuHeatLoss).minOption // PART ONE

            val minHeatLossInFront: Option[Int] = nextPositionsForBestPosition
                .filter(_.coord == finishCoord)
                .filter(_.lastDirectionMulti >= 4)
                .map(_.accuHeatLoss).minOption // PART TWO

            /*
            if(minHeatLossInFront.size > 0) {

                val xxxx = nextPositionsForBestPosition
                    .filter(_.coord == finishCoord)
                    .filter(_.lastDirectionMulti >= 4)


                println(123)
            }

             */

            val newBestOverallHeatLossFound: Int = if(
                (minHeatLossInFront.isDefined && minHeatLossInFront.get < bestOverallHeatLossFound)
            )
                minHeatLossInFront.get
            else
                bestOverallHeatLossFound

            val newRest = rest.filter(pos => pos.accuHeatLoss + pos.coord.distanceToFinish < newBestOverallHeatLossFound)

            walker2(nextPositionsForBestPosition ++ newRest, newBestOverallHeatLossFound)
        }
    }

    //val startBestOverallHeatLossFound: Int = (1 to maxGrid).flatMap(i => Vector(Coord(i, i), Coord(i, i-1))).map(coord => inputGrid(coord.y)(coord.x)).sum

    val startBestOverallHeatLossFound: Int = 100000000

    println(s"startBestOverallHeatLossFound: ${startBestOverallHeatLossFound}")

    val result = walker2(Vector(startPosition), startBestOverallHeatLossFound)

    println(s"result: ${result}")
}