package jruumis.adventofcode.year2022

import scala.util.matching.Regex

object Day19Robots2 extends App {
    val blueprintsRaw = scala.io.Source.fromFile("./Sources/Day19Robots_TEST.txt").getLines().toVector

    val blueprintPattern: Regex = """Blueprint ([0-9]+): Each ore robot costs ([0-9]+) ore. Each clay robot costs ([0-9]+) ore. Each obsidian robot costs ([0-9]+) ore and ([0-9]+) clay. Each geode robot costs ([0-9]+) ore and ([0-9]+) obsidian.""".r

    abstract class Resources {
        val ore: Int
        val clay: Int
        val obsidian: Int
        val geode: Int
    }

    case class RobotProduction(ore: Int, clay: Int, obsidian: Int, geode: Int) extends Resources {
        def addOreRobot: RobotProduction = RobotProduction(ore + 1, clay, obsidian, geode)

        def addClayRobot: RobotProduction = RobotProduction(ore, clay + 1, obsidian, geode)

        def addObsidianRobot: RobotProduction = RobotProduction(ore, clay, obsidian + 1, geode)

        def addGeodeRobot: RobotProduction = RobotProduction(ore, clay, obsidian, geode + 1)
    }

    case class RobotCost(ore: Int, clay: Int, obsidian: Int, geode: Int) extends Resources

    case class Storage(ore: Int, clay: Int, obsidian: Int, geode: Int) extends Resources {
        def +(other: Resources): Storage = Storage(ore + other.ore, clay + other.clay, obsidian + other.obsidian, geode + other.geode)

        def -(other: Resources): Storage = Storage(ore - other.ore, clay - other.clay, obsidian - other.obsidian, geode - other.geode)

        def canByRobot(rc: RobotCost): Boolean = if (ore - rc.ore >= 0 && clay - rc.clay >= 0 && obsidian - rc.obsidian >= 0 && geode - rc.geode >= 0) true else false

        def buyRobot(rc: RobotCost): Storage = this - rc
    }


    case class Blueprint(id: Int, oreCostOre: Int, clayCostOre: Int, obsCostOre: Int, obsCostClay: Int, geodeCostOre: Int, geodeCostObs: Int) {
        val oreRobotCost: RobotCost = RobotCost(oreCostOre, 0, 0, 0)
        val clayRobotCost: RobotCost = RobotCost(clayCostOre, 0, 0, 0)
        val obsidianRobotCost: RobotCost = RobotCost(obsCostOre, obsCostClay, 0, 0)
        val geodeRobotCost: RobotCost = RobotCost(geodeCostOre, 0, geodeCostObs, 0)
    }

    val blueprints: Vector[Blueprint] = blueprintsRaw.map(br => br match {
        case blueprintPattern(b, oreCostOre, clayCostOre, obsCostOre, obsCostClay, geodeCostOre, geodeCostObs) =>
            Blueprint(b.toInt, oreCostOre.toInt, clayCostOre.toInt, obsCostOre.toInt, obsCostClay.toInt, geodeCostOre.toInt, geodeCostObs.toInt)
    })


    val maxTime: Int = 24
    //val maxTime: Int = 32

    var maxGeodesInTime: scala.collection.mutable.Map[Int, Int] = scala.collection.mutable.Map()
    //var iii: Int = 0
    var cache: scala.collection.mutable.Map[(Int, RobotProduction, Storage, Option[RobotCost]), Int] = scala.collection.mutable.Map()

    def getGeode(blueprint: Blueprint, currentTime: Int, currentRobotProduction: RobotProduction, currentStorage: Storage /*, traceStack: Vector[(Int, RobotProduction, Storage, Option[RobotCost])] */): Int /*(Int, Vector[(Int, RobotProduction, Storage, Option[RobotCost])])*/ = {

        val newStorage: Storage = currentStorage //+ currentRobotProduction

        /*
        val continue: Boolean = if( !(maxGeodesInTime.keySet contains currentTime) ) {
            maxGeodesInTime(currentTime) = newStorage.geode
            true
        } else if (maxGeodesInTime(currentTime) < newStorage.geode) {
            maxGeodesInTime(currentTime) = newStorage.geode
            true
        } else if (maxGeodesInTime(currentTime) > newStorage.geode) {
            false
        } else if (maxGeodesInTime(currentTime) == newStorage.geode) {
            true
        } else false

         */

        if (currentTime == maxTime /*|| !continue*/ ) {
            //if(newStorage.geode == 22) {
            //println("here")
            //} else {}

            //(currentStorage.geode, traceStack)
            currentStorage.geode
        } else {

            val productionAndStorage: Vector[(RobotProduction, Storage, Option[RobotCost])] = Vector(

                if (currentStorage.canByRobot(blueprint.geodeRobotCost))
                    Some((
                        currentRobotProduction.addGeodeRobot,
                        currentStorage.buyRobot(blueprint.geodeRobotCost),
                        Some(blueprint.geodeRobotCost)
                    ))
                else None,

                if (currentStorage.canByRobot(blueprint.obsidianRobotCost))
                    Some((
                        currentRobotProduction.addObsidianRobot,
                        currentStorage.buyRobot(blueprint.obsidianRobotCost),
                        Some(blueprint.obsidianRobotCost)
                    ))
                else None,


                if (currentStorage.canByRobot(blueprint.clayRobotCost))
                    Some((
                        currentRobotProduction.addClayRobot,
                        currentStorage.buyRobot(blueprint.clayRobotCost),
                        Some(blueprint.clayRobotCost)
                    ))
                else None,

                if (currentStorage.canByRobot(blueprint.oreRobotCost))
                    Some((
                        currentRobotProduction.addOreRobot,
                        currentStorage.buyRobot(blueprint.oreRobotCost),
                        Some(blueprint.oreRobotCost)
                    ))
                else None,

                Some((currentRobotProduction, currentStorage, None)) // do nothing

            ).filter(_.isDefined).map(_.get)

            //val i = 1

            //val newStorage2: Storage = currentStorage + currentRobotProduction


            productionAndStorage.map { case (prod, storage, cost) =>

                //println("----------------------------------")
                //println(  (traceStack :+ (currentTime+1, prod, storage + currentRobotProduction)).mkString("\n") )

                //iii += 1
                //universalStack.append((currentTime+1, prod, storage + currentRobotProduction, cost))



                if (!(cache.keySet contains(currentTime + 1, prod, storage + currentRobotProduction, cost))) {
                    val mmm = getGeode(blueprint, currentTime + 1, prod, storage + currentRobotProduction /*, traceStack :+ (currentTime+1, prod, storage + currentRobotProduction, cost)*/)

                    cache((currentTime + 1, prod, storage + currentRobotProduction, cost)) = mmm

                    mmm
                } else {
                    cache((currentTime + 1, prod, storage + currentRobotProduction, cost))
                }

            }.max // maxBy{a => a._1}
        }
    }

    //println(blueprints.mkString("\n"))

    //val b1 = blueprints(6)

    val xxx = blueprints.take(3).map(b => {
        println(s"Processing blueprint ${b.id}...")
        //val (maxGeode, trace) = getGeode(b1, 0, RobotProduction(1,0,0,0), Storage(0,0,0,0), Vector((0, RobotProduction(1,0,0,0), Storage(0,0,0,0), None)) )
        val maxGeode = getGeode(b, 0, RobotProduction(1, 0, 0, 0), Storage(0, 0, 0, 0))

        cache = scala.collection.mutable.Map()
        maxGeodesInTime = scala.collection.mutable.Map()

        println(maxGeode)
        //println(trace.mkString("\n"))

        //println(iii)
        //println(cache.size)
        //println(cache.distinct.size)

        b.id * maxGeode

    }).sum

    println(s"Quality: ${xxx}")


    //println(trace.mkString("\n"))


}
