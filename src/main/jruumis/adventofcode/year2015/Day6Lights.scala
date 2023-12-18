package jruumis.adventofcode.year2015

import scala.util.matching.Regex

object Day6Lights extends App {

    val lightsRaw: Vector[String] = scala.io.Source.fromFile("./Sources/2015/Day6Lights.txt").getLines().toVector

    val togglePattern: Regex = """toggle ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)""".r
    val turnOnPattern: Regex = """turn on ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)""".r
    val turnOffPattern: Regex = """turn off ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)""".r

    //abstract sealed class Action
    //object Action
    //{
    //    case object Toggle
    //    case object TurnOn
    //    case object TurnOff
    //}

    case class Coord(y: Int, x: Int) {
        def inSlice(coord1: Coord, coord2: Coord): Boolean = y >= coord1.y &&  y <= coord2.y && x >= coord1.x && x <= coord2.x
    }
    case class Light(coord: Coord, on: Boolean) {
        def toggle: Light = Light(coord, !on)
        def switchOn: Light = Light(coord, true)
        def switchOff: Light = Light(coord, false)
    }

    abstract class LightAction {
        val from: Coord
        val to: Coord
    }
    case class Toggle(from: Coord, to: Coord) extends LightAction
    case class TurnOn(from: Coord, to: Coord) extends LightAction
    case class TurnOff(from: Coord, to: Coord) extends LightAction

    val lightActions: Vector[LightAction] = lightsRaw.map(lr => lr match {
        case togglePattern(a,b,c,d) => Toggle(Coord(a.toInt, b.toInt), Coord(c.toInt, d.toInt))
        case turnOnPattern(a,b,c,d) => TurnOn(Coord(a.toInt, b.toInt), Coord(c.toInt, d.toInt))
        case turnOffPattern(a,b,c,d) => TurnOff(Coord(a.toInt, b.toInt), Coord(c.toInt, d.toInt))
    })

    //println(lightActions.mkString("\n"))

    val revLightActions: Vector[LightAction] = lightActions.reverse

    def isCoordOn(c: Coord, lightActions: Vector[LightAction], accuToggles: Int): Boolean = lightActions match {
        case TurnOn(from, to) +: _ if c.inSlice(from, to) => accuToggles % 2 == 0
        case TurnOff(from, to) +: _ if c.inSlice(from, to) => accuToggles % 2 == 1
        case Toggle(from, to) +: rest if c.inSlice(from, to) => isCoordOn(c, rest, accuToggles+1)
        case _ +: rest => isCoordOn(c, rest, accuToggles)
        case Vector() => accuToggles % 2 == 1
    }

    val lightsOnVector: Vector[Boolean] = (0 to 999).toVector.flatMap(y => (0 to 999).toVector.map(x => isCoordOn(Coord(y,x), revLightActions, 0) ))
    val lightsOn: Int = lightsOnVector.filter(s => s).size

    println(s"Nr of lights on: ${lightsOn}")

    def coordBrightness(c: Coord, lightActions: Vector[LightAction], accuBrightness: Int): Int = lightActions match {
        case TurnOn(from, to) +: rest if c.inSlice(from, to) => coordBrightness(c, rest, accuBrightness+1)
        case TurnOff(from, to) +: rest if c.inSlice(from, to) => coordBrightness(c, rest, if(accuBrightness-1 < 0) 0 else accuBrightness-1)
        case Toggle(from, to) +: rest if c.inSlice(from, to) => coordBrightness(c, rest, accuBrightness+2)
        case _ +: rest => coordBrightness(c, rest, accuBrightness)
        case Vector() => accuBrightness
    }

    val lightsBrightnessVector: Vector[Int] = (0 to 999).toVector.flatMap(y => (0 to 999).toVector.map(x => coordBrightness(Coord(y,x), lightActions, 0) ))
    val lightsBrightness: Int = lightsBrightnessVector.sum

    println(s"Summary brightness: ${lightsBrightness}")

}
