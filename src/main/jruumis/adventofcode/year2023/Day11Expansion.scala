package jruumis.adventofcode.year2023

object Day11Expansion extends App {
    val space: Vector[Vector[Char]] = scala.io.Source.fromFile("./Sources/2023/Day11Expansion.txt").getLines().map(_.toVector).toVector

    val dimensionSize: Int = space.size
    val spaceInv: Vector[Vector[Char]] = (0 until dimensionSize).toVector.map(x => (0 until dimensionSize).toVector.map(y => space(y)(x)))

    val emptyLineIndexes: Vector[Long] = space.map(_.forall(_ == '.')).zipWithIndex.filter{case(b,_) => b}.map{case(_,l) => l.toLong}
    val emptyColIndexes: Vector[Long] = spaceInv.map(_.forall(_ == '.')).zipWithIndex.filter{case(b,_) => b}.map{case(_,l) => l.toLong}

    val galaxiesYX: Vector[(Int, Int)] = (0 until dimensionSize).toVector.flatMap(y => (0 until dimensionSize).toVector.map(x => {
        if(space(y)(x)=='#') Some((y,x)) else None
    })).collect{case(Some((y,x))) => (y,x)}

    val galaxyPairIndexes: Vector[(Int, Int)] = (0 until galaxiesYX.size-1).flatMap(s1 => (s1+1 until galaxiesYX.size).map(s2 => (s1,s2))).toVector

    val galaxyPairs = galaxyPairIndexes.map{case(g1,g2) => (g1, g2, galaxiesYX(g1), galaxiesYX(g2))}.map (_ match {
        case (galaxy1Index, galaxy2Index, (galaxy1y, galaxy1x),(galaxy2y,galaxy2x)) =>
            GalaxyPair(galaxy1Index, galaxy2Index, galaxy1y, galaxy1x, galaxy2y, galaxy2x)
    })

    case class GalaxyPair(galaxy1Index: Long, galaxy2Index: Long, galaxy1y: Long, galaxy1x: Long, galaxy2y: Long, galaxy2x: Long) {
        private val y1: Long = math.min(galaxy1y, galaxy2y)
        private val y2: Long = math.max(galaxy1y, galaxy2y)
        private val x1: Long = math.min(galaxy1x, galaxy2x)
        private val x2: Long = math.max(galaxy1x, galaxy2x)

        private val distanceY: Long = y2 - y1
        private val distanceX: Long = x2 - x1

        private def expandedY(expanse: Long): Long = distanceY + (expanse-1) * emptyLineIndexes.filter(c => c > y1 && c < y2).size
        private def expandedX(expanse: Long): Long = distanceX + (expanse-1) * emptyColIndexes.filter(l => l > x1 && l < x2).size
        def expandedManhattanDistance(expanse: Long): Long = expandedX(expanse) + expandedY(expanse)
    }

    // Part One
    println(s"Distances between galaxies with simple expansion: ${galaxyPairs.map(_.expandedManhattanDistance(2)).sum}")

    // Part Two
    println(s"Distances between galaxies with 1000000x expansion: ${galaxyPairs.map(_.expandedManhattanDistance(1000000L)).sum}")
}