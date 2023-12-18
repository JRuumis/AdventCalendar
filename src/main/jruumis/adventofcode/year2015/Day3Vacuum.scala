package jruumis.adventofcode.year2015

object Day3Vacuum extends App {

    val moves: Vector[Char] = scala.io.Source.fromFile("./Sources/2015/Day3Vacuum.txt").toVector

    case class Coord(y: Int, x: Int) {
        def +(other: Coord): Coord = Coord(y+other.y, x+other.x)
        def move(m: Char): Coord = m match {
            case '>' => this + Coord(0,1)
            case '<' => this + Coord(0,-1)
            case '^' => this + Coord(-1,0)
            case 'v' => this + Coord(1,0)
        }
    }

    def path(moves: Vector[Char], currentCoord: Coord = Coord(0,0), accuPath: Set[Coord] = Set(Coord(0,0))): Set[Coord] = moves match {
        case Vector() => accuPath
        case m +: rest => {
            val newCoord: Coord = currentCoord.move(m)
            path(rest, newCoord, accuPath + newCoord)
        }
    }

    val coordPath = path(moves)

    println(s"The number of houses visited: ${coordPath.size}")

    val indexedMoves: Vector[(Char, Int)] = moves.zipWithIndex
    val santaMoves = indexedMoves.filter{case(_,b) => b % 2 == 0}.map{case(a,_) => a}
    val roboSantaMoves = indexedMoves.filter{case(_,b) => b % 2 == 1}.map{case(a,_) => a}

    //println(santaMoves)
    //println(roboSantaMoves)

    val coordPathSanta: Set[Coord] = path(santaMoves)
    val coordPathRoboSanta: Set[Coord] = path(roboSantaMoves)

    println(s"Santa and RoboSanta team visited ${(coordPathSanta ++ coordPathRoboSanta).size} houses.")
}
