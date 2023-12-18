package jruumis.adventofcode.year2022

import scala.util.matching.Regex

object Day16Valves extends App {

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

    val totalTime: Int = 26

    case class Valve(name: ValveID, flowPerTimeWhenOpen: Int, isOpen: Boolean = false, actualSummaryFlow: Int = 0) {
        val flowPerTime: Int = if (isOpen) 0 else flowPerTimeWhenOpen

        def potential(currentTime: Int): Int = flowPerTimeWhenOpen * (totalTime - currentTime)

        def open(currentTime: Int) = Valve(name, flowPerTimeWhenOpen, true, potential(currentTime))
    }

    val valves: Map[String, Valve] = valveFlows.map { case (name, flow) => name -> Valve(name, flow) }
    val unopenedValveNames: Set[String] = valves.values.toSet.filter(_.flowPerTime > 0).map(v => v.name)

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

    //def getLamePotential(valvesToCheck: Set[Valve], time: Int): Int = valvesToCheck.map(_.potential(time)).sum // zero distances assumed - lame

    def getSmartPotential(currentValveName: String, unopenedValvesToCheck: Set[String], currentValves: Map[String, Valve], currentTime: Int): Int = {

        val potentialsMinusReachCosts: Set[Int] = unopenedValvesToCheck.map(n => currentValves(n)).map(v => {
            val pmd = v.potential(currentTime) - distanceBetweenValves(currentValveName)(v.name)
            if (pmd < 0) 0 else pmd
        }).filter(_ > 0)

        val sequencePenalties: Int = (0 to potentialsMinusReachCosts.size - 1).sum // because potential Valves can only be visited one after another

        potentialsMinusReachCosts.sum - sequencePenalties
    }


    def cartesian[T](va: Vector[T], vb: Vector[T]): Vector[(T, T)] = {
        va.flatMap(a => vb.map(b => (a, b)))
    }

    var currentGlobalMaxAccuFlow: Int = 0 // :-(

    var cache: scala.collection.mutable.Map[((Valve, Int, Option[(String, Valve)]), (Valve, Int, Option[(String, Valve)]), Int, Map[String, Valve]), Int] = scala.collection.mutable.Map()

    case class ForNextFlowStep(
                                  currentValve: Valve,
                                  accuFlow: Int,
                                  plusCurrentValves: Option[(String, Valve)],
                                  minusCurrentUnopenedValves: Option[String],
                                  visitedSinceLastOpen: Set[String]
                              ) {
        val forCache: (Valve, Int, Option[(String, Valve)]) = (currentValve, accuFlow, plusCurrentValves)
    }

    //@tailrec
    def getFlow(
                   step1: ForNextFlowStep,
                   step2: ForNextFlowStep,
                   currentTime: Int,
                   currentValvesFromLast: Map[String, Valve],
                   currentUnopenedValvesFromLast: Set[String]
               ): Int = {

        val currentUnopenedValves: Set[String] = (step1.minusCurrentUnopenedValves, step2.minusCurrentUnopenedValves) match {
            case (Some(a), Some(b)) => currentUnopenedValvesFromLast - a - b
            case (Some(a), None) => currentUnopenedValvesFromLast - a
            case (None, Some(b)) => currentUnopenedValvesFromLast - b
            case (None, None) => currentUnopenedValvesFromLast
        }

        val currentValves: Map[String, Valve] = (step1.plusCurrentValves, step2.plusCurrentValves) match {
            case (Some(a), Some(b)) => currentValvesFromLast + a + b
            case (Some(a), None) => currentValvesFromLast + a
            case (None, Some(b)) => currentValvesFromLast + b
            case (None, None) => currentValvesFromLast
        }

        val currentValve1: Valve = currentValves(step1.currentValve.name)
        val currentValve2: Valve = currentValves(step2.currentValve.name)

        val currentUnopenedValvesAdjustedFor1: Set[String] = currentUnopenedValves.map(uvn => currentValves(uvn)).map(uv => {
            val pmd: Int = uv.potential(currentTime) - distanceBetweenValves(currentValve1.name)(uv.name)
            (uv.name, pmd)
        }).filter { case (_, pmd) => pmd > 0 }.map { case (uvn, _) => uvn }

        val currentUnopenedValvesAdjustedFor2: Set[String] = currentUnopenedValves.map(uvn => currentValves(uvn)).map(uv => {
            val pmd: Int = uv.potential(currentTime) - distanceBetweenValves(currentValve2.name)(uv.name)
            (uv.name, pmd)
        }).filter { case (_, pmd) => pmd > 0 }.map { case (uvn, _) => uvn }


        val accuFlow: Int = step1.accuFlow + step2.accuFlow
        currentGlobalMaxAccuFlow = if (accuFlow > currentGlobalMaxAccuFlow) accuFlow else currentGlobalMaxAccuFlow


        (step1, step2, currentTime) match {

            case _ if (step1.visitedSinceLastOpen contains step1.currentValve.name) || (step2.visitedSinceLastOpen contains step2.currentValve.name) => accuFlow

            case (_, _, 26) => accuFlow

            case _ if currentUnopenedValves.isEmpty || currentUnopenedValvesAdjustedFor1.isEmpty || currentUnopenedValvesAdjustedFor2.isEmpty => accuFlow

            case _ if (
                accuFlow +
                    getSmartPotential(step1.currentValve.name, currentUnopenedValves, currentValves, currentTime + 1) +
                    getSmartPotential(step2.currentValve.name, currentUnopenedValves, currentValves, currentTime + 1)
                ) <= currentGlobalMaxAccuFlow => accuFlow


            case _ => {

                val moveToValvesFrom1: Vector[Valve] = valveTunnels(step1.currentValve.name).map(tunnelToValve => currentValves(tunnelToValve)).sortBy(y => -getSmartPotential(y.name, currentUnopenedValves, currentValves, currentTime))
                val moveToValvesFrom2: Vector[Valve] = valveTunnels(step2.currentValve.name).map(tunnelToValve => currentValves(tunnelToValve)).sortBy(y => -getSmartPotential(y.name, currentUnopenedValves, currentValves, currentTime))

                val stepsFrom1: Vector[ForNextFlowStep] = moveToValvesFrom1.map(v => ForNextFlowStep(v, step1.accuFlow, None, None, step1.visitedSinceLastOpen + step1.currentValve.name))
                val stepsFrom2: Vector[ForNextFlowStep] = moveToValvesFrom2.map(v => ForNextFlowStep(v, step2.accuFlow, None, None, step2.visitedSinceLastOpen + step2.currentValve.name))


                val openCurrent1: Vector[ForNextFlowStep] = if (currentValve1.flowPerTime > 0) { // currentValve1 can be opened

                    val newValve = currentValve1.open(currentTime + 1)
                    val newAccuFlow = step1.accuFlow + newValve.actualSummaryFlow
                    val plusCurrentValves = Some(newValve.name -> newValve) // to replace
                    val minusUnopenedValves = Some(newValve.name)

                    val newStep = ForNextFlowStep(
                        currentValve = newValve,
                        accuFlow = newAccuFlow,
                        plusCurrentValves = plusCurrentValves,
                        minusCurrentUnopenedValves = minusUnopenedValves,
                        visitedSinceLastOpen = Set()
                    )

                    Vector(newStep)
                } else {
                    Vector()
                }


                val openCurrent2: Vector[ForNextFlowStep] = if (currentValve2.flowPerTime > 0 && currentValve2.name != currentValve1.name) { // currentValve2 can be opened, if not he same as valve1

                    val newValve = currentValve2.open(currentTime + 1)
                    val newAccuFlow = step2.accuFlow + newValve.actualSummaryFlow
                    val plusCurrentValves = Some(newValve.name -> newValve) // to replace
                    val minusUnopenedValves = Some(newValve.name)

                    val newStep = ForNextFlowStep(
                        currentValve = newValve,
                        accuFlow = newAccuFlow,
                        plusCurrentValves = plusCurrentValves,
                        minusCurrentUnopenedValves = minusUnopenedValves,
                        visitedSinceLastOpen = Set()
                    )

                    Vector(newStep)
                } else {
                    Vector()
                }

                val stepPairs: Vector[(ForNextFlowStep, ForNextFlowStep)] = cartesian(stepsFrom1 ++ openCurrent1, stepsFrom2 ++ openCurrent2)


                val allFlows: Vector[Int] = stepPairs.map {
                    case (s1, s2) => {

                        if (!(cache.keySet contains(s1.forCache, s2.forCache, currentTime + 1, currentValves))) {

                            val xxx: Int = getFlow(s1, s2, currentTime + 1, currentValves, currentUnopenedValves)

                            cache((s1.forCache, s2.forCache, currentTime + 1, currentValves)) = xxx

                            xxx
                        } else {
                            //println("----")
                            cache((s1.forCache, s2.forCache, currentTime + 1, currentValves))
                        }


                    }
                }

                allFlows.max
            }

        }
    }

    println("Seeking Max Flow...")

    val p1 = ForNextFlowStep(
        currentValve = valves("AA"),
        accuFlow = 0,
        plusCurrentValves = None,
        minusCurrentUnopenedValves = None,
        visitedSinceLastOpen = Set()
    )


    val p2 = ForNextFlowStep(
        currentValve = valves("AA"),
        accuFlow = 0,
        plusCurrentValves = None,
        minusCurrentUnopenedValves = None,
        visitedSinceLastOpen = Set()
    )


    val xxx = getFlow(p1, p2, 0, valves, unopenedValveNames)
    println(xxx)


    // for TEST: 1707

}
