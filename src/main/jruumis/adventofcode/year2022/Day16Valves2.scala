package jruumis.adventofcode.year2022

import scala.util.matching.Regex

object Day16Valves2 extends App {

    val valvesInputRaw: Vector[String] = scala.io.Source.fromFile("./Sources/Day16Valves_TEST.txt").getLines.toVector

    val valvesPattern: Regex = "Valve ([A-Z][A-Z]) has flow rate=([0-9]+); tunnel(?:[s]?) lead(?:[s]?) to valve(?:[s]?) ([A-Z, ]+)".r
    val valvesParsed: Vector[(String, Int, Vector[String])] = valvesInputRaw.map(vr => vr match {
        case valvesPattern(v, r, ov) => (v, r.toInt, ov.split(", ").toVector)
    })

    type ValveID = String


    val valveNames: Set[ValveID] = valvesParsed.map { case (valve, _, _) => valve }.toSet
    val valveFlows: Map[ValveID, Int] = valvesParsed.map { case (valve, flow, _) => valve -> flow }.toMap
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

    //println(distanceBetweenValves.mkString("\n"))


    case class Valve(id: ValveID, potentialFlow: Int, actualFlow: Int = 0) {

        def open(time: Int): Valve = Valve(id, 0, potentialFlow * (maxTime - time /* +1 */))

        def potential(time: Int, distance: Int): Int = {
            val p = potentialFlow * (maxTime - time /* +1 */) //- distance // !!!!!!!!!!!!!!!!!!!!!
            if (p < 0) 0 else p
        }

        def potentialFrom(sourceValve: Valve, time: Int): Int = {
            val distance: Int = distanceBetweenValves(sourceValve.id)(this.id)
            potential(time, distance)
        }

        def otherValveIds: Set[ValveID] = distanceBetweenValves(id).keySet

        def currentPotentialSumToOtherValves(currentValves: Map[ValveID, Valve], time: Int): Int = {
            otherValveIds.map(vid => currentValves(vid)).map(_.potentialFrom(this, time)).sum
        }

        def otherValvesSortedByPotential(currentValves: Map[ValveID, Valve], time: Int): Vector[Valve] = {
            otherValveIds
                .map(vid => currentValves(vid)).toVector
                .map(v => (v, v.potentialFrom(this, time)))
                .filter { case (_, potential) => potential > 0 }
                .sortBy { case (_, potential) => -potential }
                .map { case (v, _) => v }
        }

        def getDistanceTo(other: ValveID): Int = {
            distanceBetweenValves(this.id)(other)
        }
    }

    sealed abstract class Agent()

    object Agent {
        final case object Human extends Agent()

        final case object Elephant extends Agent()
    }

    sealed abstract class Action()

    object Action {
        final case object Open extends Action()

        final case object Move extends Action()

        final case object Idle extends Action()

        final case object Break extends Action()
    }

    case class AgentQueue(agent: Agent, action: Action, valveId: ValveID, inTime: Int, ignoreEmptyDestination: Boolean = false) {
        def nextTick: AgentQueue = AgentQueue(agent, action, valveId, inTime - 1)
    }


    var currentBestSummaryFlow: scala.collection.mutable.Map[Vector[ValveID], Int] = scala.collection.mutable.Map()
    var maxFlow: Int = 0
    var minTimeToAllOpen: Int = 1000


    def cartesian[T](va: Vector[T], vb: Vector[T]): Vector[(T, T)] = {
        va.flatMap(a => vb.map(b => (a, b)))
    }


    def flow(aq1: AgentQueue, aq2: AgentQueue, currentValves: Map[ValveID, Valve], currentTime: Int, accuFlow: Int): Int = (aq1, aq2) match {

        case _ if currentTime == maxTime =>
            accuFlow

        case (AgentQueue(_, Action.Idle, _, _, _), AgentQueue(_, Action.Idle, _, _, _)) => accuFlow

        //        case _ if currentValves(aq1.valveId).currentPotentialSumToOtherValves(currentValves, currentTime) + currentValves(aq2.valveId).currentPotentialSumToOtherValves(currentValves, currentTime) == 0 =>
        //            accuFlow

        case (AgentQueue(_, _, _, inTime1, _), AgentQueue(_, _, _, inTime2, _)) if inTime1 > 0 && inTime2 > 0 => {
            flow(aq1.nextTick, aq2.nextTick, currentValves, currentTime + 1, accuFlow)
        }

        case (AgentQueue(a1, action1, valveId1, inTime1, ignoreEmpty1), AgentQueue(a2, action2, valveId2, inTime2, ignoreEmpty2)) if inTime1 == 0 || inTime2 == 0 => {

            // -= 1 =-
            val (newAgentQueues1, newCurrentValves1, newAccuFlow1): (Vector[AgentQueue], Map[ValveID, Valve], Int) = (action1, inTime1) match {

                case (Action.Idle, _) =>
                    (Vector(AgentQueue(a1, Action.Idle, valveId1, 0)), currentValves, accuFlow)

                case (_, t) if t > 0 =>
                    (Vector(aq1.nextTick), currentValves, accuFlow)

                case (Action.Move, 0) => { // destination reached
                    if (currentValves(valveId1).potentialFlow == 0)
                        (Vector(AgentQueue(a1, Action.Idle, valveId1, 0)), currentValves, accuFlow)
                    else {
                        (Vector(AgentQueue(a1, Action.Open, valveId1, 1)), currentValves, accuFlow)
                    }
                }

                case (Action.Open, 0) => { // ready to open
                    if (currentValves(valveId1).potentialFlow == 0 && ignoreEmpty1) {
                        val currentValve1: Valve = currentValves(valveId1)

                        val canMoveTo: Vector[Valve] = currentValve1.otherValvesSortedByPotential(currentValves, currentTime)
                        val nextMoves: Vector[AgentQueue] = canMoveTo.map(valveToMoveTo => AgentQueue(a1, Action.Move, valveToMoveTo.id, currentValve1.getDistanceTo(valveToMoveTo.id)))

                        (nextMoves, currentValves, accuFlow)
                    } else if (currentValves(valveId1).potentialFlow == 0)
                        (Vector(AgentQueue(a1, Action.Idle, valveId1, 0)), currentValves, accuFlow)
                    else {
                        val currentValve1: Valve = currentValves(valveId1)
                        val currentValveOpen1: Valve = currentValve1.open(currentTime)

                        val newCurrentValves: Map[ValveID, Valve] = currentValves + (currentValveOpen1.id -> currentValveOpen1)
                        val newAccuFlow: Int = accuFlow + currentValveOpen1.actualFlow

                        //if (maxFlow < newAccuFlow)
                        //    maxFlow = newAccuFlow
                        //else {}

                        val canMoveTo: Vector[Valve] = currentValveOpen1.otherValvesSortedByPotential(newCurrentValves, currentTime)
                        val nextMoves: Vector[AgentQueue] = canMoveTo.map(valveToMoveTo => AgentQueue(a1, Action.Move, valveToMoveTo.id, currentValveOpen1.getDistanceTo(valveToMoveTo.id)))

                        val nextMovesAdjusted: Vector[AgentQueue] = if (nextMoves == Vector()) {
                            Vector(AgentQueue(a1, Action.Idle, valveId1, 0))
                        } else {
                            nextMoves
                        }

                        (nextMovesAdjusted, newCurrentValves, newAccuFlow)
                    }
                }

            }

            // -= 2 =-
            val (newAgentQueues2, newCurrentValves2, newAccuFlow2): (Vector[AgentQueue], Map[ValveID, Valve], Int) = (action2, inTime2) match {

                case (Action.Idle, _) =>
                    (Vector(AgentQueue(a2, Action.Idle, valveId2, 0)), newCurrentValves1, newAccuFlow1)

                case (_, t) if t > 0 =>
                    (Vector(aq2.nextTick), newCurrentValves1, newAccuFlow1)

                case (Action.Move, 0) => { // destination reached
                    if (newCurrentValves1(valveId2).potentialFlow == 0)
                        (Vector(AgentQueue(a2, Action.Idle, valveId2, 0)), newCurrentValves1, newAccuFlow1)
                    else {
                        (Vector(AgentQueue(a2, Action.Open, valveId2, 1)), newCurrentValves1, newAccuFlow1)
                    }
                }

                case (Action.Open, 0) => { // ready to open
                    if (newCurrentValves1(valveId2).potentialFlow == 0 && ignoreEmpty2) {
                        val currentValve2: Valve = newCurrentValves1(valveId2)

                        val canMoveTo: Vector[Valve] = currentValve2.otherValvesSortedByPotential(newCurrentValves1, currentTime)
                        val nextMoves: Vector[AgentQueue] = canMoveTo.map(valveToMoveTo => AgentQueue(a2, Action.Move, valveToMoveTo.id, currentValve2.getDistanceTo(valveToMoveTo.id)))

                        (nextMoves, newCurrentValves1, newAccuFlow1)
                    } else if (newCurrentValves1(valveId2).potentialFlow == 0)
                        (Vector(AgentQueue(a2, Action.Idle, valveId2, 0)), newCurrentValves1, newAccuFlow1)
                    else {
                        val currentValve2: Valve = newCurrentValves1(valveId2)
                        val currentValveOpen2: Valve = currentValve2.open(currentTime)

                        val newCurrentValves: Map[ValveID, Valve] = newCurrentValves1 + (currentValveOpen2.id -> currentValveOpen2)
                        val newAccuFlow: Int = newAccuFlow1 + currentValveOpen2.actualFlow

                        //if (maxFlow < newAccuFlow)
                        //    maxFlow = newAccuFlow
                        //else {}

                        val canMoveTo: Vector[Valve] = currentValveOpen2.otherValvesSortedByPotential(newCurrentValves, currentTime)
                        val nextMoves: Vector[AgentQueue] = canMoveTo.map(valveToMoveTo => AgentQueue(a2, Action.Move, valveToMoveTo.id, currentValveOpen2.getDistanceTo(valveToMoveTo.id)))

                        val nextMovesAdjusted: Vector[AgentQueue] = if (nextMoves == Vector()) {
                            Vector(AgentQueue(a2, Action.Idle, valveId1, 0))
                        } else {
                            nextMoves
                        }

                        (nextMovesAdjusted, newCurrentValves, newAccuFlow)
                    }
                }

            }

            if (newAgentQueues1.isEmpty || newAgentQueues2.isEmpty) { // !!!!!!!! <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< allow empty. only when both empty then abandon
                println("ERROR")
                newAccuFlow2 // dead end reached
            } else {

                val newOpenValveIds: Vector[ValveID] = newCurrentValves2.values.toVector.filter(_.actualFlow > 0).map(_.id).sorted


                /*
                val searchCanProceed: Boolean = if(newOpenValveIds == Vector())
                    true
                else if( !(currentBestSummaryFlow.keySet contains newOpenValveIds) ) {
                    currentBestSummaryFlow(newOpenValveIds) = newAccuFlow2
                    true
                } else if(currentBestSummaryFlow(newOpenValveIds) <= newAccuFlow2) {
                    currentBestSummaryFlow(newOpenValveIds) = newAccuFlow2
                    true
                } else if(currentBestSummaryFlow(newOpenValveIds) > newAccuFlow2) {
                    false
                } else true


                 */




                if (true) {
                    //if(searchCanProceed) {
                    val newAgentQueues: Vector[(AgentQueue, AgentQueue)] = cartesian(newAgentQueues1, newAgentQueues2)

                    //println(s"${" " * currentTime}${currentTime}: ${newAccuFlow2}")

                    if (maxFlow < newAccuFlow2)
                        maxFlow = newAccuFlow2
                    else {}

                    val xxx = newCurrentValves2.values.map(_.actualFlow).sum
                    if (xxx != newAccuFlow2) {
                        println(s"ERROR: ${xxx} vs ${newAccuFlow2}")
                    } else {}

                    val vvv = newCurrentValves2.values.map(a => if (a.actualFlow > 0) 1 else 0).sum
                    if (vvv == 6) {

                        if (minTimeToAllOpen > currentTime)
                            minTimeToAllOpen = currentTime
                        else {}

                        //println(currentTime)
                    }

                    //                    if(maxFlow < xxx)
                    //                        maxFlow = xxx
                    //                    else {}


                    newAgentQueues.map { case (q1, q2) =>
                        flow(q1, q2, newCurrentValves2, currentTime + 1, newAccuFlow2)
                    }.max
                } else newAccuFlow2
            }
        }

    }

    val startAction1: AgentQueue = AgentQueue(Agent.Human, Action.Open, "AA", 0, ignoreEmptyDestination = true)
    val startAction2: AgentQueue = AgentQueue(Agent.Elephant, Action.Open, "AA", 0, ignoreEmptyDestination = true)

    val startValves: Map[ValveID, Valve] = valveFlows.map { case (a, b) => (a -> Valve(a, b)) }
    val startTime = 0
    val startFlow = 0


    println("Searching...")
    val xxx = flow(startAction1, startAction2, startValves, startTime, startFlow)

    println(xxx)

    println(maxFlow)

    println(minTimeToAllOpen)

}
