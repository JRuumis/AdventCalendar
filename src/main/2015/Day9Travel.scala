import scala.annotation.tailrec
import scala.util.matching.Regex

object Day9Travel extends App {

    val routesRaw: Vector[String] = scala.io.Source.fromFile("./Sources/2015/Day9Routes.txt").getLines().toVector

    val routePattern: Regex = """([A-Za-z]+) to ([A-Za-z]+) = ([0-9]+)""".r
    val routes: Vector[(Set[String], Int)] = routesRaw.map{ route => route match {
        case routePattern(from, to, distance) => (Set(from,to),distance.toInt)
    }}

    val routesMap = routes.map{case(v,_) => v}.reduce(_ ++ _)
        .map(v => (v -> routes.filter{case(a,_) => a contains v}.map{case(a,b)=> ((a - v).head, b)}))
        .toMap

    @tailrec
    def travel(currentVertex: String, alreadyVisited: Set[String], sortCoef: Int = 1, accuPath: Int = 0): Int = {
        val possibleRoutes = routesMap(currentVertex).filter{case(v,_) => !(alreadyVisited contains v)}

        if (possibleRoutes.size == 0)
            accuPath
        else {
            val (nextVertex, pathToNextVertex) = possibleRoutes.sortBy{case(_,d) => d * sortCoef}.head
            travel(nextVertex, alreadyVisited + nextVertex, sortCoef, accuPath+pathToNextVertex)
        }
    }

    val part1PathLength: Int = routesMap.keySet.map(v => travel(v, Set(v))).min
    println(s"The shortest route between destinations is : ${part1PathLength}")

    val part2PathLength: Int = routesMap.keySet.map(v => travel(v, Set(v), sortCoef = -1)).max
    println(s"The longest route between destinations is : ${part2PathLength}")
}