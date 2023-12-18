package jruumis.adventofcode.year2022

import scala.util.matching.Regex

object Day16Valves3 extends App {

    val valvesInputRaw: Vector[String] = scala.io.Source.fromFile("./Sources/Day16Valves.txt").getLines.toVector

    val valvesPattern: Regex = "Valve ([A-Z][A-Z]) has flow rate=([0-9]+); tunnel(?:[s]?) lead(?:[s]?) to valve(?:[s]?) ([A-Z, ]+)".r
    val valvesParsed: Vector[(String, Int, Vector[String])] = valvesInputRaw.map(vr => vr match {
        case valvesPattern(v, r, ov) => (v, r.toInt, ov.split(", ").toVector)
    })

    type ValveID = String


    val valveNames: Set[ValveID] = valvesParsed.map { case (valve, _, _) => valve }.toSet
    val valveFlows: Map[ValveID, Int] = valvesParsed.map { case (valve, flow, _) => valve -> flow }.toMap
    val valvesWithNonZeroFlows: Set[ValveID] = valveFlows.filter { case (_, b) => b > 0 }.keySet
    val valvesToValves: Map[ValveID, Vector[ValveID]] = valvesParsed.map { case (valve, _, tunnels) => valve -> tunnels }.toMap

    val maxTime: Int = 26


    // distances
    def getShortestDistances(currentValveName: ValveID, curDist: Int, leftValves: Set[ValveID], accuDistances: Set[(ValveID, Int)] = Set()): Set[(ValveID, Int)] = {
        val valveDistance = (currentValveName, curDist)
        val newAccuDistances = accuDistances + valveDistance

        if (leftValves.isEmpty) newAccuDistances
        else {
            val valvesToVisit = valvesToValves(currentValveName).toSet.filter { f => leftValves contains f }

            if (valvesToVisit.isEmpty) newAccuDistances
            else valvesToVisit.flatMap(vv => getShortestDistances(vv, curDist + 1, leftValves - currentValveName, newAccuDistances))
        }
    }

    println("Calculating shortest distances between all Valves...")
    val distanceBetweenValves: Map[ValveID, Map[ValveID, Int]] = valveNames.map(currentValveName => {
        val otherValves = valveNames - currentValveName

        (
            currentValveName,
            getShortestDistances(currentValveName, 0, otherValves)
                .groupBy { case (s, _) => s }
                .map { case (a, b) => (a, b.toVector.map { case (_, b) => b }.sorted.head) }
        )
    }).map { case (a, b) => (a -> b.removed(a)) }.toMap

    println("Distances between valves calculated.")

    def walk(currentValveID: ValveID, currentTime: Int, currentFlow: Int, valvesVisited: Set[ValveID], valvesExcluded: Set[ValveID], accu: Set[(Set[ValveID], Int)]): Set[(Set[ValveID], Int)] = {

        val timeLeft: Int = maxTime - currentTime
        val nextValvesToVisit: Set[ValveID] = (valvesWithNonZeroFlows -- valvesVisited -- valvesExcluded).filter(valveIdTo => distanceBetweenValves(currentValveID)(valveIdTo) < timeLeft)

        if (nextValvesToVisit == Set())
            accu + ((valvesVisited, currentFlow))
        else
            nextValvesToVisit.map(nextValve => {
                val newTime: Int = currentTime + distanceBetweenValves(currentValveID)(nextValve) + 1
                val newFlow: Int = currentFlow + (timeLeft - distanceBetweenValves(currentValveID)(nextValve)) * valveFlows(nextValve)

                walk(nextValve, newTime, newFlow, valvesVisited + nextValve, valvesExcluded, accu + ((valvesVisited, currentFlow)))
            }).reduce(_ ++ _)
    }

    def bestWalks(currentValveID: ValveID, valvesExcluded: Set[ValveID] = Set()): Set[(Set[ValveID], Int)] = {
        val allWalks: Set[(Set[ValveID], Int)] = walk(currentValveID, 1, 0, Set(), valvesExcluded, Set())
        val bestWalks: Set[(Set[ValveID], Int)] = allWalks.groupBy { case (a, _) => a }.map { case (a, b) => (a, b.map { case (x, y) => y }.max) }.toSet
        bestWalks
    }


    println("Calculating valve walks for Human...")
    val bestValveWalks = bestWalks("AA")

    println(s"${bestValveWalks.size} walks found for Human.")
    println("Calculating Elephant walks for each Human walk...")

    val humanAndElephantWalks = bestValveWalks.map { case (v1, f1) => (v1, f1, bestWalks("AA", v1)) }
    val HumanWithBestElephantWalk = humanAndElephantWalks.map { case (v1, f1, b2) => {
        val (v2, f2) = b2.toVector.sortBy { case (_, b) => -b }.head
        (v1, f1, v2, f2, f1 + f2)
    }
    }

    println("Elephant walks calculated.")
    //println(HumanWithBestElephantWalk.mkString("\n"))

    val maxSumOfBestHumanAndElephant: Int = HumanWithBestElephantWalk.map { case (a, b, c, d, e) => e }.max

    println(s"Max best Human + Elephant walk: ${maxSumOfBestHumanAndElephant}")
}
