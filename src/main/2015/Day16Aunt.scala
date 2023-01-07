import scala.util.matching.Regex

object Day16Aunt extends App {

    val auntsRaw: Vector[String] = scala.io.Source.fromFile("./Sources/2015/Day16Aunt.txt").getLines().toVector

    val auntPattern: Regex = """Sue ([0-9]+): ([a-z]+): ([0-9]+), ([a-z]+): ([0-9]+), ([a-z]+): ([0-9]+)""".r
    val aunts: Vector[(Int, Map[String, Int])] = auntsRaw.map(_ match {
        case auntPattern(a,b,c,d,e,f,g) => (a.toInt -> Map( (b -> c.toInt), (d -> e.toInt), (f -> g.toInt) ))
    })

    val senderAunt: Map[String, Int] = Map(
        "children" -> 3,
        "cats" -> 7,
        "samoyeds" -> 2,
        "pomeranians" -> 3,
        "akitas" -> 0,
        "vizslas" -> 0,
        "goldfish" -> 5,
        "trees" -> 3,
        "cars" -> 2,
        "perfumes" -> 1
    )

    val comparedAunts = aunts.map{ case(auntId, auntItems) => {
        val partOneCompare: Boolean = auntItems.map{case(i,n) => senderAunt(i) == n}.reduce(_ && _)

        val partTwoCompare: Boolean = auntItems.map{case(i,n) => i match {
            case "cats" => senderAunt(i) < n
            case "trees" => senderAunt(i) < n
            case "pomeranians" => senderAunt(i) > n
            case "goldfish" => senderAunt(i) > n
            case _ => senderAunt(i) == n
        }}.reduce(_ && _)

        (auntId, auntItems, partOneCompare, partTwoCompare)
    }}

    val partOneAunt: Int = comparedAunts.filter{case(_,_,m,_) => m}.map{case(a,_,_,_) => a}.head
    val partTwoAunt: Int = comparedAunts.filter{case(_,_,_,m) => m}.map{case(a,_,_,_) => a}.head

    println(s"Part 1: Sender aunt: ${partOneAunt}")
    println(s"Part 2: Sender aunt: ${partTwoAunt}")
}