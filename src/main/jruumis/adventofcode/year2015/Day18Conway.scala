package jruumis.adventofcode.year2015

import scala.annotation.tailrec

object Day18Conway extends App {

    type Grid = Vector[Vector[Char]]
    val lifeInput: Vector[Vector[Char]] = scala.io.Source.fromFile("./Sources/2015/Day18Conway.txt").getLines().map(_.toVector).toVector

    val maxY = lifeInput.length
    val maxX = lifeInput(0).length
    val maxGenerations: Int = 100

    case class Coord(y: Int, x: Int) {
        def +(other: Coord): Coord = Coord(y + other.y, x + other.x)
    }

    val alwaysOn: Set[Coord] = Set(Coord(0,0), Coord(0,maxX-1), Coord(maxY-1, 0), Coord(maxY-1, maxX-1)) // for Part 2

    val neighbourCoordinates: Map[Coord, Set[Coord]] = (0 to maxY-1).toVector.flatMap(y => (0 to maxX-1).toVector.map(x => {
        val neighboursRangeY: Set[Int] = ((if(y==0) 0 else -1) to (if(y==maxY-1) 0 else 1)).toSet
        val neighboursRangeX: Set[Int] = ((if(x==0) 0 else -1) to (if(x==maxX-1) 0 else 1)).toSet
        val neighbourRelativeCoords: Set[Coord] = neighboursRangeY.flatMap(ny => neighboursRangeX.map(nx => Coord(ny,nx)))

        (Coord(y,x) -> (neighbourRelativeCoords.map(n => Coord(y,x) + n) - Coord(y,x)))
    })).toMap

    def neighbourLightsOn(inputCoord: Coord, grid: Grid, alwaysOn: Set[Coord]): Int = {
        neighbourCoordinates(inputCoord).toVector
            .map(c => if(alwaysOn contains Coord(c.y,c.x)) '#' else grid(c.y)(c.x) )
            .filter(_ == '#')
            .size
    }

    @tailrec
    def iterate(currentGrid: Grid, alwaysOn: Set[Coord] = Set(), generation: Int = 0): Grid = {
        if(generation == maxGenerations) currentGrid
        else {
            val newGrid: Grid = (0 to maxY-1).toVector.map(y => (0 to maxX-1).toVector.map(x => currentGrid(y)(x) match {
                case _ if alwaysOn contains Coord(y,x) => '#'
                case '.' if 3 == neighbourLightsOn(Coord(y,x), currentGrid, alwaysOn) => '#'
                case '#' if Set(2,3) contains neighbourLightsOn(Coord(y,x), currentGrid, alwaysOn) => '#'
                case _ => '.'
            }))

            iterate(newGrid, alwaysOn, generation+1)
        }
    }

    def gridLightsOn(g: Grid): Int = g.flatten.filter(_ == '#').size

    val conwaySimple: Grid = iterate(lifeInput)
    //println(conwaySimple.map(_.mkString).mkString("\n"))
    println(s"Lights on after ${maxGenerations} generations is ${gridLightsOn(conwaySimple)}")

    val conwayWithAlwaysOn: Grid = iterate(lifeInput, alwaysOn)
    //println(conwayWithAlwaysOn.map(_.mkString).mkString("\n"))
    println(s"Lights on after ${maxGenerations} generations with Always On lights is ${gridLightsOn(conwayWithAlwaysOn)}")
}