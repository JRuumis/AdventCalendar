package jruumis.adventofcode.coordinates

case class Coord(x: Long, y: Long) {
    def +(other: Coord): Coord = Coord(x + other.x, y + other.y)
    def *(mutliplier: Long): Coord = Coord(x * mutliplier, y * mutliplier)

    def manhattanDistanceBetween(other: Coord): Long = Math.abs(x - other.x) + Math.abs(y - other.y)
    def euclideanDistanceBetween(other: Coord): Double = Math.sqrt(Math.pow(x - other.x, 2) + Math.pow(y - other.y, 2))

    def isOrthogonalWith (other: Coord): Boolean = x == other.x || y == other.y
    def isDiagonalWith (other: Coord): Boolean = Math.abs(x - other.x) == Math.abs(y - other.y)
}
object Coord {
    def manhattanDistanceBetween(coord1: Coord, coord2: Coord): Long = coord1 manhattanDistanceBetween coord2
    def euclideanDistanceBetween(coord1: Coord, coord2: Coord): Double = coord1 euclideanDistanceBetween coord2
}
