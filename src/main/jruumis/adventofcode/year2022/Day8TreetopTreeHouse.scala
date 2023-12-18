package jruumis.adventofcode.year2022

import scala.annotation.tailrec

object Day8TreetopTreeHouse extends App {

    val forestInput: List[List[Int]] = scala.io.Source.fromFile("./Sources/Day8TreetopTreeHouse.txt").getLines().toList.map(_.toList.map(_.asDigit))

    // Part 1
    val forestWithCoords = forestInput.map(_.zipWithIndex).transpose.map(_.zipWithIndex).transpose.map(l => l.map { case (((t, x), y)) => (t, x, y) })

    @tailrec
    def getVisible(
                      treesToProcess: List[(Int, Int, Int)],
                      visibleTrees: List[(Int, Int, Int)] = List(),
                      currentMaxHeight: Int = -1
                  ): List[(Int, Int, Int)] = treesToProcess match {
        case Nil => visibleTrees
        case _ if currentMaxHeight == 9 => visibleTrees // max height
        case (currentHeight, x, y) :: rest if currentHeight > currentMaxHeight => getVisible(rest, (currentHeight, x, y) :: visibleTrees, currentHeight)
        case (currentHeight, _, _) :: rest if currentHeight <= currentMaxHeight => getVisible(rest, visibleTrees, currentMaxHeight)
    }

    val visibles = (forestWithCoords ::: forestWithCoords.transpose)
        .flatMap(treeLine => getVisible(treeLine) ::: getVisible(treeLine.reverse))
        .distinct

    println(s"Toe overall number of visible trees is: ${visibles.size}")

    // Part 2
    val treeArray: Array[Array[Int]] = forestInput.map(_.toArray).toArray
    val sizeX: Int = treeArray(0).size
    val sizeY: Int = treeArray.size

    @tailrec
    def treeSurvey(treeHeight: Int, xCoord: Int, yCoord: Int, stepX: Int, stepY: Int, accu: Int = 0): Int = (xCoord + stepX, yCoord + stepY) match {
        case (x, y) if x < 0 || x >= sizeX || y < 0 || y >= sizeY => accu
        case (x, y) if treeArray(y)(x) >= treeHeight => accu + 1
        case (x, y) if treeArray(y)(x) < treeHeight => treeSurvey(treeHeight, x, y, stepX, stepY, accu + 1)
    }

    val treeScenicScores = (0 to sizeY - 1).map(y => (0 to sizeX - 1).map(x => (
        treeSurvey(treeArray(y)(x), x, y, 1, 0) *
            treeSurvey(treeArray(y)(x), x, y, -1, 0) *
            treeSurvey(treeArray(y)(x), x, y, 0, 1) *
            treeSurvey(treeArray(y)(x), x, y, 0, -1)
        )).toList).toList

    val maxScenicScore = treeScenicScores.map(_.max).max

    println(s"The maximum scenic score in the forest is ${maxScenicScore}")
}
