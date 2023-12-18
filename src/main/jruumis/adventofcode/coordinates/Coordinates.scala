package jruumis.adventofcode.coordinates

import scala.annotation.tailrec

case class Coordinates(coords: Vector[Coord]) {
    lazy val minX: Long = coords.map(_.x).min
    lazy val maxX: Long = coords.map(_.x).max
    lazy val minY: Long = coords.map(_.y).min
    lazy val maxY: Long = coords.map(_.y).max

    lazy val normalised = {
        if(minX == 0 && minY == 0) this
        else {
            val newCoords: Vector[Coord] = coords.map(c => Coord(c.x - minX, c.y - minY))
            Coordinates(newCoords)
        }
    }

    //lazy val polygon

    private lazy val coordsLoop: Vector[Coord] = if(coords.head == coords.last) coords else coords :+ coords.head

    lazy val isManhattanPolygon: Boolean = Coordinates.coordPairChecker( (a,b) => a isOrthogonalWith b, _ && _ , coordsLoop).getOrElse(false)
    lazy val isOrthogonalPolygon: Boolean = isManhattanPolygon

    lazy val loopBorderLength: Option[Long] = if(isManhattanPolygon) Some(Coordinates.coordPairReducer(coordsLoop, Coord.manhattanDistanceBetween, _ + _, 0L)) else None

    lazy val shoelaceAreaManhattan: Option[Long] = if(isManhattanPolygon) Some(Coordinates.shoelacer(coordsLoop, coordsLoop.head)) else None
    lazy val innerHalfAreaManhattan: Option[Long] = if (loopBorderLength.isDefined) Some(((loopBorderLength.get - 4) / 2) + (0.25 * 4).toLong) else None // 0.25 * 4 represents the 4 outer corners and 1/4 from each
    lazy val outerHalfAreaManhattan: Option[Long] = if (loopBorderLength.isDefined) Some(((loopBorderLength.get - 4) / 2) + (0.75 * 4).toLong) else None // 0.75 * 4 represents the 4 inner corners and 3/4 from each
    lazy val fillAreaManhattan: Option[Long] = if(shoelaceAreaManhattan.isDefined && innerHalfAreaManhattan.isDefined) Some(shoelaceAreaManhattan.get - innerHalfAreaManhattan.get) else None
    lazy val borderAreaManhattan: Option[Long] = if(innerHalfAreaManhattan.isDefined && outerHalfAreaManhattan.isDefined) Some(innerHalfAreaManhattan.get + outerHalfAreaManhattan.get) else None
    lazy val fullAreaManhattan: Option[Long] = if(borderAreaManhattan.isDefined && fillAreaManhattan.isDefined) Some(borderAreaManhattan.get + fillAreaManhattan.get) else None
}
object Coordinates {
    @tailrec
    private def coordPairChecker[T](
                            checker: (Coord, Coord) => T,
                            reducer: (T, T) => T,
                            coords: Vector[Coord],
                            accu: Option[T] = None
                        ): Option[T] = coords match {
        case Vector() => accu
        case a +: Vector() => accu
        case a +: b +: rest if accu.isEmpty => coordPairChecker(checker, reducer, b +: rest, Some(checker(a, b)))
        case a +: b +: rest if accu.isDefined => coordPairChecker(checker, reducer, b +: rest, Some(reducer(accu.get, checker(a, b))))
    }

    @tailrec
    private def coordPairReducer[T](
                            coords: Vector[Coord],
                            pairExtractor: (Coord, Coord) => T,
                            reducer: (T, T) => T,
                            accu: T
                        ): T = coords match {
        case Vector() => accu
        case a +: Vector() => accu
        case a +: b +: rest => coordPairReducer(b +: rest, pairExtractor, reducer, reducer(accu, pairExtractor(a, b)))
    }

    @tailrec
    private def shoelacer(coords: Vector[Coord], firstCoord: Coord, accu: Long = 0): Long = coords match {
        case Vector() => accu.abs / 2
        case a +: b +: rest => shoelacer(b +: rest, firstCoord, accu + (a.x.toLong * b.y.toLong - a.y.toLong * b.x.toLong))
        case a +: Vector() => shoelacer(Vector(), firstCoord, accu + (a.x.toLong * firstCoord.y.toLong - a.y.toLong * firstCoord.x.toLong))
    }

    @tailrec
    def getCoordsFromDirectionsWithSteps(
                                                 directionsSteps: Vector[(Direction, Int)],
                                                 startCoord: Coord = Coord(0,0),
                                                 accuCoords: Vector[Coord] = Vector(Coord(0,0))
                 ): Coordinates = directionsSteps match {
        case Vector() => Coordinates(accuCoords)
        case (dir, steps) +: rest => {
            val nextCoord: Coord = startCoord + (dir.coord * steps)
            getCoordsFromDirectionsWithSteps(rest, nextCoord, accuCoords :+ nextCoord)
        }
    }

    def getCoordsFromDirections(
                                   directions: Vector[Direction],
                                   startCoord: Coord = Coord(0,0)
                               ): Coordinates = getCoordsFromDirectionsWithSteps( directions.map( (_,1) ), startCoord )

}
