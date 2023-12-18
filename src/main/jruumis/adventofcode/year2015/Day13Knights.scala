package jruumis.adventofcode.year2015

import scala.math.Ordering.Implicits.seqOrdering
import scala.util.matching.Regex

object Day13Knights extends App {

    val knightsRaw: Vector[String] = scala.io.Source.fromFile("./Sources/2015/Day13Knights.txt").getLines().toVector

    val knightGainPattern: Regex = """([A-Za-z]+) would gain ([0-9]+) happiness units by sitting next to ([A-Za-z]+)\.""".r
    val knightLosePattern: Regex = """([A-Za-z]+) would lose ([0-9]+) happiness units by sitting next to ([A-Za-z]+)\.""".r

    val knightsParsed = knightsRaw.map(_ match {
        case knightGainPattern(a,b,c) => (a, b.toInt, c)
        case knightLosePattern(a,b,c) => (a, -b.toInt, c)
    })

    type KnightName = String
    case class Knight(knight: KnightName, neighbours: Set[KnightName]) {
        def happiness(knightsMap: Map[String, Map[String, Int]]): Int = neighbours.map(n => knightsMap(knight)(n)).sum
    }

    def permutate(left: Set[String], currentRoot: Vector[String] = Vector()): Set[Vector[String]] = left match {
        case s if s.isEmpty => Set(currentRoot)
        case s => s.map(e => permutate(left - e, currentRoot :+ e)).reduce(_ ++ _)
    }

    val knightsMap1: Map[String, Map[String, Int]] = knightsParsed.groupBy{case(a,_,_) => a}.map{case(k,v) => (k -> v.map{case(_,b,c) => (c -> b)}.toMap)}
    val knightsMap2: Map[String, Map[String, Int]] = knightsMap1.map{case(k -> s) => (k -> (s + ("Janis" -> 0)))} + ("Janis" -> knightsMap1.keySet.map(k => (k -> 0)).toMap)

    def getMaxHappiness(knightsMap: Map[String, Map[String, Int]]): Int = {
        val knightPermutations: Set[Vector[String]] = permutate(knightsMap.keySet)

        val indexes: Set[(Int, Int, Int)] = (0 to knightsMap.size-1).map(i => (i, if(i-1 < 0) knightsMap.size-1 else i-1, if(i+1 > knightsMap.size-1) 0 else i+1)).toSet

        val knightTables: Set[Set[Knight]] = knightPermutations.map(kv => indexes.map{ case(k,l,r) => Knight(kv(k), Set(kv(l), kv(r))) })
        val maxHappiness: Int = knightTables.map(k => k.map(_.happiness(knightsMap)).sum).max

        maxHappiness
    }

    println(s"Max possible happiness at the table, part 1: ${getMaxHappiness(knightsMap1)}")
    println(s"Max possible happiness at the table, part 2: ${getMaxHappiness(knightsMap2)}")
}