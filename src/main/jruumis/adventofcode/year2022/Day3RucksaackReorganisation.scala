package jruumis.adventofcode.year2022

object Day3RucksaackReorganisation extends App {

    val itemCodes = (('a' to 'z').toList zip (1 to 26).toList) ::: (('A' to 'Z').toList zip (27 to 52).toList)
    val itemCodesMap = itemCodes.map { case (i, c) => i -> c }.toMap

    val rucksackContent = scala.io.Source.fromFile("./Sources/Day3RucksaackReorganisation.txt").getLines().toList

    def midSplit(s: String): (String, String) = (s.substring(0, s.length / 2), s.substring(s.length / 2, s.length))

    val rucksackContentSplits = rucksackContent.map(midSplit(_))
    val rucksackDupes = rucksackContentSplits.map { case (a, b) => a.toCharArray intersect b.toCharArray }.map(_ (0)) // deduplicate by taking first element

    val rucksackDupePriorities = rucksackDupes.map(itemCodesMap(_))
    val rucksackSummaryPriority = rucksackDupePriorities.sum

    println(s"Rucksask summary priority: ${rucksackSummaryPriority}")

    val elfGroups = rucksackContent.grouped(3).toList

    def badge(l: List[String]): Char = l match {
        case List(a, b, c) => (a.toCharArray intersect b.toCharArray intersect c.toCharArray) (0)
    }

    val elfGroupBadges = elfGroups.map(badge(_))
    val elfGroupBadgeCodes = elfGroupBadges.map(itemCodesMap(_))
    val elfGroupSummary = elfGroupBadgeCodes.sum

    println(s"Elf group Priority summary: ${elfGroupSummary}")
}
