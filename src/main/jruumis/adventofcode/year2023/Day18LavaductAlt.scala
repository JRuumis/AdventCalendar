package jruumis.adventofcode.year2023

import jruumis.adventofcode.coordinates.Coordinates.getCoordsFromDirectionsWithSteps
import jruumis.adventofcode.coordinates.{Coordinates, Direction, Down, Left, Right, Up}

object Day18LavaductAlt extends App {
    val inputRaw: Vector[String] = scala.io.Source.fromFile("./Sources/2023/Day18Lavaduct.txt").getLines.toVector

    def parseDirection(dir: String): Direction = dir match {
        case "U" | "3" => Up
        case "D" | "1" => Down
        case "L" | "2" => Left
        case "R" | "0" => Right
    }

    val inputInstructions: Vector[Instruction] = inputRaw.collect{
        case s"$dir $stepsString (#$colourString)" => Instruction(parseDirection(dir), stepsString.toInt, Colour(colourString))
    }

    case class Instruction(direction: Direction, steps: Int, colour: Colour)

    case class Colour(rgb: String) {
       val stepsFromColour: Long = Integer.parseInt(rgb.take(5), 16)
       val directionFromColour: Direction = parseDirection(rgb.last.toString)
    }

    // Part One
    val directionsStepsPartOne: Vector[(Direction, Int)] = inputInstructions.map(i => (i.direction, i.steps))
    val areaPartOne: Option[Long] = getCoordsFromDirectionsWithSteps(directionsStepsPartOne).fullAreaManhattan
    println(s"Lava area is ${if(areaPartOne.isDefined) areaPartOne.get else "not defined. Is the polygon a Manhattan (orthogonal) polygon?"}")

    // Part Two
    val directionsStepsPartTwo: Vector[(Direction, Int)] = inputInstructions.map(i => (i.colour.directionFromColour, i.colour.stepsFromColour.toInt))
    val areaPartTwo: Option[Long] = getCoordsFromDirectionsWithSteps(directionsStepsPartTwo).fullAreaManhattan
    println(s"Lava area from colour is ${if(areaPartTwo.isDefined) areaPartTwo.get else "not defined. Is the polygon a Manhattan (orthogonal) polygon?"}")
}