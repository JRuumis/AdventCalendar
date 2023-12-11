object Day11Expansion extends App {
    val space: Vector[Vector[Char]] = scala.io.Source.fromFile("./Sources/2023/Day11Expansion.txt").getLines().map(_.toVector).toVector

    val dimensionSize: Int = space.size
    val spaceInv: Vector[Vector[Char]] = (0 until dimensionSize).toVector.map(x => (0 until dimensionSize).toVector.map(y => space(y)(x)))

    val emptyLines: Vector[Long] = space.map(_.forall(_ == '.')).zipWithIndex.filter{case(b,_) => b}.map{case(_,l) => l.toLong}
    val emptyCols: Vector[Long] = spaceInv.map(_.forall(_ == '.')).zipWithIndex.filter{case(b,_) => b}.map{case(_,l) => l.toLong}

    val starsYX: Vector[(Int, Int)] = (0 until dimensionSize).toVector.flatMap(y => (0 until dimensionSize).toVector.map(x => {
        if(space(y)(x)=='#') Some((y,x)) else None
    })).collect{case(Some((y,x))) => (y,x)}

    val starPairIndexes: Vector[(Int, Int)] = (0 until starsYX.size-1).flatMap(s1 => (s1+1 until starsYX.size).map(s2 => (s1,s2))).toVector

    val starPairs = starPairIndexes.map{case(s1,s2) => (s1, s2, starsYX(s1), starsYX(s2))}.map (_ match {
        case (star1Index, star2Index, (star1y, star1x),(star2y,star2x)) =>
            StarPair(star1Index, star2Index, star1y, star1x, star2y, star2x)
    })

    case class StarPair(star1Index: Long, star2Index: Long, star1y: Long, star1x: Long, star2y: Long, star2x: Long) {
        private val y1: Long = math.min(star1y, star2y)
        private val y2: Long = math.max(star1y, star2y)
        private val x1: Long = math.min(star1x, star2x)
        private val x2: Long = math.max(star1x, star2x)

        private val distanceY: Long = y2 - y1
        private val distanceX: Long = x2 - x1

        private def expandedY(expanse: Long): Long = distanceY + (expanse-1) * emptyLines.filter(c => c > y1 && c < y2).size
        private def expandedX(expanse: Long): Long = distanceX + (expanse-1) * emptyCols.filter(l => l > x1 && l < x2).size
        def expandedManhattanDistance(expanse: Long): Long = expandedX(expanse) + expandedY(expanse)
    }

    // Part One
    println(s"Distances of starts with simple expansion: ${starPairs.map(_.expandedManhattanDistance(2)).sum}")

    // Part Two
    println(s"Distances of starts with 1000000x expansion: ${starPairs.map(_.expandedManhattanDistance(1000000L)).sum}")
}