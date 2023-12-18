package jruumis.adventofcode.year2022

import scala.util.matching.Regex

object Day16Analysis extends App {

    val valvesInputRaw: Vector[String] = scala.io.Source.fromFile("./Sources/Day16Valves_TEST.txt").getLines.toVector

    val valvesPattern: Regex = "Valve ([A-Z][A-Z]) has flow rate=([0-9]+); tunnel(?:[s]?) lead(?:[s]?) to valve(?:[s]?) ([A-Z, ]+)".r
    val valvesParsed: Vector[(String, Int, Vector[String])] = valvesInputRaw.map(vr => vr match {
        case valvesPattern(v, r, ov) => (v, r.toInt, ov.split(", ").toVector)
    })

    val valveFlows: Map[String, Int] = valvesParsed.map { case (valve, flow, _) => valve -> flow }.toMap

    val valvesWithFlows = valveFlows.map { case (a, b) => a -> s"${a}__${b}" }

    val valveTunnels: Map[String, Vector[String]] = valvesParsed.map { case (valve, _, tunnels) => valve -> tunnels }.toMap
    val valveTunnelsExp = valveTunnels.toVector.flatMap { case (v, ve) => ve.map(i => Vector(v, i).sorted) }.distinct

    val valveTunelsBoom = valveTunnelsExp.map { case (Vector(a, b)) => s"${valvesWithFlows(a)} -- ${valvesWithFlows(b)};" }

    println(valveTunelsBoom.mkString("\n"))


    val valveNames: Set[String] = valvesParsed.map { case (valve, _, _) => valve }.toSet

    // https://bit.ly/3uVRmr4


    def getShortestDistances(currentValveName: String, curDist: Int, leftValves: Set[String], accuDistances: Set[(String, Int)] = Set()): Set[(String, Int)] = {
        val valveDistance = (currentValveName, curDist)
        val newAccuDistances = accuDistances + valveDistance

        if (leftValves.isEmpty) newAccuDistances
        else {
            val valvesToVisit = valveTunnels(currentValveName).toSet.filter { f => leftValves contains f }

            //println(s"${"\t".repeat(curDist)}${currentValveName} - ${leftValves} - ${valvesToVisit}")

            if (valvesToVisit.isEmpty) newAccuDistances
            else valvesToVisit.flatMap(vv => getShortestDistances(vv, curDist + 1, leftValves - currentValveName, newAccuDistances))
        }
    }


    println("Calculating shortest distances between all Valves...")
    val distanceBetweenValves: Map[String, Map[String, Int]] = valveNames.map(currentValveName => {
        val otherValves = valveNames - currentValveName

        (
            currentValveName,
            getShortestDistances(currentValveName, 0, otherValves).groupBy { case (s, _) => s }.map { case (a, b) => (a, b.toVector.map { case (a, b) => b }.sorted.head) }
        )
    }).toMap

    println(distanceBetweenValves.mkString("\n"))
}
