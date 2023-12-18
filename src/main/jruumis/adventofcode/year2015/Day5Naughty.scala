package jruumis.adventofcode.year2015

object Day5Naughty extends App {

    val niceRaw: Vector[String] = scala.io.Source.fromFile("./Sources/2015/Day5Naughty.txt").getLines().toVector

    def vowelCount(s: String): Int = {
        val vowels: Set[Char] = "aeiou".toSet
        s.map(c => if(vowels contains c) 1 else 0).sum
    }

    def hasDoubleLetter(s: List[Char], prevChar: Char = ' '): Boolean = s match {
        case Nil => false
        case c :: _ if prevChar == c => true
        case c :: rest => hasDoubleLetter(rest, c)
    }

    def checkForbiddenStrings(s: String): Boolean = {
        val forbiddenStrings = Vector("ab", "cd", "pq", "xy")
        !(forbiddenStrings.map(s contains _).reduce(_ || _))
    }

    def naughtyOrNice(s: String): Boolean =
        vowelCount(s) >= 3 &&
        hasDoubleLetter(s.toList) &&
        checkForbiddenStrings(s)

    val niceNaughty: Vector[Boolean] = niceRaw.map(s => naughtyOrNice(s))
    val niceCount: Int = niceNaughty.filter(b => b).size

    println(s"Nr of nice strings: ${niceCount}")


    def pairSearch(s: Vector[Char]): Boolean = s match {
        case c1 +: c2 +: rest if rest.mkString contains Vector(c1,c2) => true
        case _ +: Vector() => false
        case _ +: c2 +: rest => pairSearch(c2 +: rest)
    }

    def repeatOneInBetween(s: Vector[Char]): Boolean = s match {
        case a +: _ +: c +: _ if a == c => true
        case _ +: b +: c +: rest => repeatOneInBetween(b +: c +: rest)
        case _ => false
    }

    def naughtyOrNice2(s: String): Boolean =
        pairSearch(s.toVector) && repeatOneInBetween(s.toVector)

    val niceNaughty2: Vector[Boolean] = niceRaw.map(s => naughtyOrNice2(s))
    val niceCount2: Int = niceNaughty2.filter(s => s).size

    println(s"Nr of nice strings, part 2: ${niceCount2}")
}
