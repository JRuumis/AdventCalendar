import scala.annotation.tailrec

object Day18Lavaduct extends App {
    val inputRaw = scala.io.Source.fromFile("./Sources/2023/Day18Lavaduct.txt").getLines.toVector

    def parseDirection(dir: String) = dir match {
        case "U" | "3" => Up
        case "D" | "1" => Down
        case "L" | "2" => Left
        case "R" | "0" => Right
    }
    
    val inputInstructions: Vector[Instruction] = inputRaw.map(_ match {
        case s"$dir $stepsString (#$colourString)" => Instruction(parseDirection(dir), stepsString.toInt, Colour(colourString))
    })

    case class Instruction(direction: Direction, steps: Int, colour: Colour)

    case class Coord(x: Int, y: Int) {
        def +(other: Coord): Coord = Coord(x + other.x, y + other.y)
        def *(other: Int): Coord = Coord(x * other, y * other)
    }

    sealed trait Direction { val coord: Coord }
    case object Up extends Direction { override val coord = Coord(0, -1) }
    case object Down extends Direction { override val coord = Coord(0, 1) }
    case object Left extends Direction { override val coord = Coord(-1, 0) }
    case object Right extends Direction { override val coord = Coord(1, 0) }

    case class Colour(rgb: String) {
       val stepsFromColour: Long = Integer.parseInt(rgb.take(5), 16)
       val directionFromColour: Direction = parseDirection(rgb.last.toString)
    }

    @tailrec
    def getCoords(
                     directionsSteps: Vector[(Direction, Int)],
                     curCoord: Coord = Coord(0,0),
                     accuCoords: Vector[Coord] = Vector(Coord(0,0))
                 ): Vector[Coord] = directionsSteps match {
        case Vector() => accuCoords
        case (dir, steps) +: rest => {
            val nextCoord: Coord = curCoord + (dir.coord * steps)
            getCoords(rest, nextCoord, accuCoords :+ nextCoord)
        }
    }

    @tailrec
    def shoelacer(coords: Vector[Coord], firstCoord: Coord, accu: Long = 0): Long = coords match {
        case Vector() => accu.abs / 2
        case a +: b +: rest => shoelacer(b +: rest, firstCoord, accu + (a.x.toLong * b.y.toLong - a.y.toLong * b.x.toLong))
        case a +: Vector() => shoelacer(Vector(), firstCoord, accu + (a.x.toLong * firstCoord.y.toLong - a.y.toLong * firstCoord.x.toLong))
    }
    def shoelace(coords: Vector[Coord]): Long = shoelacer(coords, coords.head)

    def getAreaFromDirectionsSteps(directionsSteps: Vector[(Direction, Int)]): Long = {
        val coords: Vector[Coord] = getCoords(directionsSteps)
        val borderLength: Long = directionsSteps.map{case(_, steps) => steps}.sum
        val shoelaceArea: Long = shoelace(coords)
        val innerHalfArea: Long = ((borderLength - 4) / 2) + (0.25 * 4).toLong
        val outerHalfArea: Long = ((borderLength - 4) / 2) + (0.75 * 4).toLong
        val fillArea: Long = shoelaceArea - innerHalfArea
        val borderArea: Long = innerHalfArea + outerHalfArea
        val fullArea: Long = borderArea + fillArea

        fullArea
    }

    val directionsStepsPartOne: Vector[(Direction, Int)] = inputInstructions.map(i => (i.direction, i.steps))
    val areaPartOne = getAreaFromDirectionsSteps(directionsStepsPartOne)
    println("Lava area: " + areaPartOne)

    val directionsStepsPartTwo: Vector[(Direction, Int)] = inputInstructions.map(i => (i.colour.directionFromColour, i.colour.stepsFromColour.toInt))
    val areaPartTwo = getAreaFromDirectionsSteps(directionsStepsPartTwo)
    println("Lava area from colour: " + areaPartTwo)
}