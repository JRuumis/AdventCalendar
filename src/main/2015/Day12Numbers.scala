import scala.util.matching.Regex

object Day12Numbers extends App {

    val numbersRaw: String = scala.io.Source.fromFile("./Sources/2015/Day12Numbers.txt").mkString

    val numPattern: Regex = """(-?[0-9]+)""".r
    val numbersSum = numPattern.findAllIn(numbersRaw).toVector.map(_.toInt).sum

    println(s"Sum of all numbers is: ${numbersSum}")


    def iterateMap(input: Vector[Char], accuString: String = "", foundRed: Boolean): (Vector[Char], String) = input match {
        case 'r' +: 'e' +: 'd' +: rest => iterateMap(rest, accuString :+ 'R' :+ 'E' :+ 'D', true)
        case '}' +: rest if !foundRed => (rest, accuString :+ '}')
        case '}' +: rest if foundRed => (rest, accuString.replaceAll("[0-9]", "X") :+ '}')
        //case '}' +: rest if foundRed => (rest, "FOUND_RED" :+ '}')
        case '{' +: rest => {
            val (newInput, addAccu) = iterateMap(rest, "", foundRed)
            iterateMap(newInput, (accuString :+ '{') + addAccu, foundRed)
        }
        case c +: rest => iterateMap(rest, accuString :+ c, foundRed)
    }

    def iterate(input: Vector[Char], accuString: String = ""): String = input match {
        case Vector() => accuString
        case '{' +: rest => {
            val (newInput, addAccu) = iterateMap(rest, "", false)
            iterate(newInput, (accuString :+ '{') + addAccu)
        }
        case c +: rest => iterate(rest, accuString :+ c)
    }

    val redRemoved: String = iterate(numbersRaw.toVector)

    println(redRemoved)

    val numbersSum2 = numPattern.findAllIn(redRemoved).toVector.map(_.toInt).sum


    println(s"Sum of all numbers with RED removed: ${numbersSum2}")

}
