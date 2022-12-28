import scala.util.matching.Regex

object Day16Valves_Part1 extends App {

    val valvesInputRaw: Vector[String] = scala.io.Source.fromFile("./Sources/Day16Valves.txt").getLines.toVector

    val valvesPattern: Regex = "Valve ([A-Z][A-Z]) has flow rate=([0-9]+); tunnel(?:[s]?) lead(?:[s]?) to valve(?:[s]?) ([A-Z, ]+)".r
    val valvesParsed: Vector[(String, Int, Vector[String])] = valvesInputRaw.map(vr => vr match {
        case valvesPattern(v, r, ov) => (v, r.toInt, ov.split(", ").toVector)
    })

    //println(valvesParsed.mkString("\n"))

    val valveNames: Set[String] = valvesParsed.map { case (valve, _, _) => valve }.toSet
    val valveFlows: Map[String, Int] = valvesParsed.map { case (valve, flow, _) => valve -> flow }.toMap
    val valveTunnels: Map[String, Vector[String]] = valvesParsed.map { case (valve, _, tunnels) => valve -> tunnels }.toMap

    //println(valveTunnels.mkString("\n"))

    type ValveID = String

    val totalTime: Int = 30

    case class Valve(name: ValveID, flowPerTimeWhenOpen: Int, isOpen: Boolean = false, actualSummaryFlow: Int = 0) {
        val flowPerTime: Int = if (isOpen) 0 else flowPerTimeWhenOpen

        def potential(currentTime: Int): Int = flowPerTimeWhenOpen * (totalTime - currentTime)

        def open(currentTime: Int) = Valve(name, flowPerTimeWhenOpen, true, potential(currentTime))
    }

    val valves: Map[String, Valve] = valveFlows.map { case (name, flow) => name -> Valve(name, flow) }
    val unopenedValves = valves.values.toSet.filter(_.flowPerTime > 0)

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

    val distanceBetweenValves: Map[String, Map[String, Int]] = valveNames.map(currentValveName => {
        val otherValves = valveNames - currentValveName

        (
            currentValveName,
            getShortestDistances(currentValveName, 0, otherValves).groupBy { case (s, _) => s }.map { case (a, b) => (a, b.toVector.map { case (a, b) => b }.sorted.head) }
        )
    }).toMap

    def getLamePotential(valvesToCheck: Set[Valve], time: Int): Int = valvesToCheck.map(_.potential(time)).sum // zero distances assumed - lame

    def getSmartPotential(currentValve: Valve, openValvesToCheck: Set[Valve], currentTime: Int): Int = {

        val openCosts = (1 to openValvesToCheck.size).sum

        val potentialsMinusReachCosts = openValvesToCheck.map(v => {
            val pd = v.potential(currentTime) - distanceBetweenValves(currentValve.name)(v.name)
            if (pd < 0) 0 else pd
        }).sum

        potentialsMinusReachCosts //- openCosts
    }


    var currentGlobalMaxAccuFlow: Int = 0 // !!!!!!!!!!!!!!!!!!!!

    //@tailrec
    def getFlow(currentValve: Valve, currentTime: Int, accuFlow: Int, currentValves: Map[String, Valve], currentUnopenedValves: Set[Valve], visitedSinceLastOpen: Set[String]): Int = {

        //println(currentGlobalMaxAccuFlow)

        (currentValve, currentTime) match {

            case (_, _) if visitedSinceLastOpen contains currentValve.name => accuFlow
            case (_, 30) => accuFlow
            case (_, _) if currentUnopenedValves.isEmpty => accuFlow
            //case (_,t) if accuFlow + getLamePotential(currentUnopenedValves, t+1) < currentGlobalMaxAccuFlow => accuFlow
            case (_, t) if accuFlow + getSmartPotential(currentValve, currentUnopenedValves, t + 1) <= currentGlobalMaxAccuFlow => accuFlow
            case (Valve(valveName, _, true, _), t) => { // already opened - move on
                valveTunnels(valveName)
                    .map(tunnelToValve => currentValves(tunnelToValve))
                    //.sortBy(y => -y.leftPotentialFlow)
                    .sortBy(y => -getSmartPotential(y, currentUnopenedValves, t))
                    .map { toValve => getFlow(toValve, t + 1, accuFlow, currentValves, currentUnopenedValves, visitedSinceLastOpen + currentValve.name) }.max
            }
            case (Valve(valveName, 0, _, _), t) => { // zero flow - move on
                valveTunnels(valveName)
                    .map(tunnelToValve => currentValves(tunnelToValve))
                    //.sortBy(y => -y.leftPotentialFlow)
                    .sortBy(y => -getSmartPotential(y, currentUnopenedValves, t))
                    .map { toValve => getFlow(toValve, t + 1, accuFlow, currentValves, currentUnopenedValves, visitedSinceLastOpen + currentValve.name) }.max
            }
            case (vlv@Valve(valveName, potentialFlow, false, 0), t) if potentialFlow > 0 => {

                // open
                val newValve = vlv.open(t + 1)
                //val newAccuFlow = accuFlow + newValve.actualFlow
                val newAccuFlow = accuFlow + newValve.actualSummaryFlow
                val newCurrentValves = currentValves + (newValve.name -> newValve)
                val newUnopenedValves = currentUnopenedValves - vlv

                currentGlobalMaxAccuFlow = if (newAccuFlow > currentGlobalMaxAccuFlow) newAccuFlow else currentGlobalMaxAccuFlow

                val newFlow: Int = getFlow(newValve, t + 1, newAccuFlow, newCurrentValves, newUnopenedValves, Set()) // newValve.name


                // don't open

                val proceedWithoutOpeningFlows: Vector[Int] = valveTunnels(valveName)
                    .map(tunnelTo => currentValves(tunnelTo))
                    //.sortBy(y => -y.leftPotentialFlow)
                    .sortBy(y => -getSmartPotential(y, currentUnopenedValves, t + 1))
                    .map { v => getFlow(v, t + 1, accuFlow, currentValves, currentUnopenedValves, visitedSinceLastOpen + currentValve.name) }




                // return
                (proceedWithoutOpeningFlows :+ newFlow).max

                //newFlow
            }
        }
    }

    println("Starting...")

    val xxx = getFlow(valves("AA"), 0, 0, valves, unopenedValves, Set())
    println(xxx)


}
