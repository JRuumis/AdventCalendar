package jruumis.adventofcode.year2023

import scala.annotation.tailrec

object Day12Springs_unrefactored extends App {

    //val inputRaw = scala.io.Source.fromFile("./Sources/2023/Day12Springs_TEST1.txt").getLines().toVector
    val inputRaw = scala.io.Source.fromFile("./Sources/2023/Day12Springs.txt").getLines().toVector

    val layoutsCounts: Vector[(Vector[Char], Vector[Int])] = inputRaw.map(_ match {
        case s"$layout $counts" => (layout.toVector, counts.split(",").map(_.toInt).toVector)
    })

    val layoutsCountsUnfolded: Vector[(Vector[Char], Vector[Int])] = inputRaw.map(_ match {
        case s"$layout $counts" => (Vector.fill(5)(layout).mkString("?").toVector, Vector.fill(5)(counts.split(",").map(_.toInt).toVector).flatten )
    })

    @tailrec
    def getFirstStreamIndexAndLength(layout: Vector[Char], firstIndex: Int = 0, firstLength: Int = 0): (Int,Int) = layout match {
        case '?' +: _ => (firstIndex, firstLength)
        case '#' +: rest => getFirstStreamIndexAndLength(rest, firstIndex, firstLength+1)
        case _ +: rest if firstLength == 0 => getFirstStreamIndexAndLength(rest, firstIndex+1, firstLength)
        case _ => (firstIndex, firstLength)
    }

    var cacheSuccess: scala.collection.mutable.Map[ (Vector[Char], Vector[Int]), Long ] = scala.collection.mutable.Map()
    var cacheBad: scala.collection.mutable.Set[ (Vector[Char], Vector[Int]) ] = scala.collection.mutable.Set()

    def searcher2(layout: Vector[Char], counts: Vector[Int], accu: Vector[Char] = Vector()): Long = {

        val knownSpringsCount: Int = layout.count(_ == '#')
        val unknownsCount: Int = layout.count(_ == '?')
        val totalSpringsCount: Int = counts.sum
        val missingSpringsCount: Int = totalSpringsCount - knownSpringsCount
        val surplusSpringsCount: Int = totalSpringsCount - knownSpringsCount - unknownsCount

        if (missingSpringsCount < 0) {
            0L
        } else if (surplusSpringsCount > 0) {
            0L
        } else {

            val firstStreamIndexAndLength = getFirstStreamIndexAndLength(layout)

            if (cacheSuccess contains (layout, counts) ) {
                cacheSuccess( (layout, counts) )
            } else
            if( cacheBad contains (layout, counts) ) {
                0
            } else
            firstStreamIndexAndLength match {
                case (_, firstLength) if firstLength > 0 && counts.isEmpty => 0L
                case (_, firstLength) if firstLength == 0 && counts.isEmpty && !layout.contains('#') => 1L
                case (_, firstLength) if firstLength == 0 && counts.isEmpty && layout.contains('#') => 0L
                case (_, firstLength) if firstLength > counts.head => 0L

                case (firstIndex, firstLength) if (
                        firstLength > 0 &&
                        firstLength < counts.head &&
                        (firstIndex + firstLength == layout.size || layout(firstIndex + firstLength) == '.')
                    ) => 0L

                case (firstIndex, firstLength) if (
                        firstLength > 0 &&
                        firstLength < counts.head &&
                        (firstIndex + firstLength == layout.size || layout(firstIndex + firstLength) == '.')
                    ) => 0L

                case (firstIndex, firstLength) if firstLength == counts.head => {

                    val followingDotLength: Int = if (firstIndex + firstLength == layout.size) 0 else 1
                    val dropper: Int = firstIndex + firstLength + followingDotLength

                    if (followingDotLength == 1 && layout(dropper - 1) == '#') {
                        0L
                    } else {
                        val toAccu = layout.take(dropper)

                        val result = searcher2(layout.drop(dropper), counts.tail, accu ++ toAccu)

                        if(result == 0) {
                            //cacheBad += ( (layout, counts) )
                            result
                        } else {
                            //cacheSuccess( (layout, counts) ) = result
                            result
                        }

                    }
                }

                case (firstIndex, firstLength) => {
                    val firstUnknownIndex: Int = layout.indexOf('?')

                    if (firstUnknownIndex > -1) {

                        val result1 = searcher2(layout.updated(firstUnknownIndex, '#'), counts, accu)
                        val result2 = searcher2(layout.updated(firstUnknownIndex, '.'), counts, accu)

                        if(result1 == 0) {
                            //cacheBad += ( (layout.updated(firstUnknownIndex, '#'), counts) )
                        } else {
                            //cacheSuccess( (layout.updated(firstUnknownIndex, '#'), counts) ) = result1
                        }

                        if(result2 == 0) {
                            //cacheBad += ( (layout.updated(firstUnknownIndex, '.'), counts) )
                        } else {
                            //cacheSuccess( (layout.updated(firstUnknownIndex, '.'), counts) ) = result2
                        }

                        if (result1 + result2 == 0) {
                            cacheBad += ( (layout, counts) )
                        } else {
                            cacheSuccess( (layout, counts) ) = result1 + result2
                        }

                        result1 + result2

                    } else {
                        0L
                    }
                }
            }
        }
    }

    val t1 = System.nanoTime()

    //val totalCombos: Vector[Long] = layoutsCounts.map{ case(layout,counts) => {
    val totalCombos: Vector[Long] = layoutsCountsUnfolded.map{ case(layout,counts) => {

        val knownSpringsCount: Int = layout.count(_ == '#')
        val totalSpringsCount: Int = counts.sum
        val missingSpringsCount: Int = totalSpringsCount - knownSpringsCount

        if (missingSpringsCount == 0) {
            1L
        } else {
            val nrOfCombos = searcher2(layout, counts)

            //println(s"Sub: ${nrOfCombos} in ${durationSeconds} seconds")

            cacheBad = scala.collection.mutable.Set()
            cacheSuccess = scala.collection.mutable.Map()

            nrOfCombos
        }
    }}

    val t2 = System.nanoTime()
    val durationSeconds: Double = ((t2 - t1) / 1e9)

    println(s"TOTAL SUM: ${totalCombos.sum} in ${durationSeconds} seconds")
}