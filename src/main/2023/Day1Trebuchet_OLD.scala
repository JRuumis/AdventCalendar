import scala.annotation.tailrec

object Day1Trebuchet_OLD extends App {

    val calibrations: Vector[String] = scala.io.Source.fromFile("./Sources/2023/Day1Trebuchet_TEST2.txt").getLines().toVector

    @tailrec
    def calibrationValues(calibRaw: List[Char], curFirst: Option[Char] = None, curLast: Option[Char] = None): Int = calibRaw match {
        case i :: rest if i.isDigit => calibrationValues(rest, if(curFirst.isDefined) curFirst else Some(i), Some(i))
        case i :: rest => calibrationValues(rest, curFirst, curLast)
        case Nil if curFirst.isDefined && curLast.isDefined => curFirst.get.asDigit * 10 + curLast.get.asDigit
        case _ => 0
    }

    val part1: Int = calibrations.map(c => calibrationValues(c.toList)).sum

    println(part1)




}
