object Day1Trebuchet extends App {
    val calibrations: Vector[String] = scala.io.Source.fromFile("./Sources/2023/Day1Trebuchet.txt").getLines().toVector

    case class Digit(digitString: String, digitInt: Int)

    val allDigitsPartOne: Vector[Digit] = Vector(
        Digit("1", 1),
        Digit("2", 2),
        Digit("3", 3),
        Digit("4", 4),
        Digit("5", 5),
        Digit("6", 6),
        Digit("7", 7),
        Digit("8", 8),
        Digit("9", 9)
    )

     val additionalDigitsPartTwo: Vector[Digit] = Vector(
        Digit("one", 1),
        Digit("two", 2),
        Digit("three", 3),
        Digit("four", 4),
        Digit("five", 5),
        Digit("six", 6),
        Digit("seven", 7),
        Digit("eight", 8),
        Digit("nine", 9),
    )

    val allDigitsPartTwo: Vector[Digit] = allDigitsPartOne ++ additionalDigitsPartTwo

    case class FirstLastOccurrence(firstDigit: Digit, firstDigitIndex: Int, lastDigit: Digit, lastDigitIndex: Int) {
        val calibrationValue: Int = 10 * firstDigit.digitInt + lastDigit.digitInt
        override def toString: String = s"first digit is ${firstDigit.digitString} at position ${firstDigitIndex}, last digit is ${lastDigit.digitString} at position ${lastDigitIndex}, calibration value: ${calibrationValue}"
    }

    def findFirstLastOccurrence(digitsToSearchFor: Vector[Digit], calibration: String): Option[FirstLastOccurrence] = {
        val firstOccurrences = digitsToSearchFor.map{d => (d, calibration.indexOf(d.digitString))}.filter{case(_,b) => b>=0}
        val lastOccurrences = digitsToSearchFor.map{d => (d, calibration.lastIndexOf(d.digitString))}.filter{case(_,b) => b>=0}

        val smallestFirstOccurrence = firstOccurrences.sortBy{case (d,i) => i}.headOption
        val biggestLastOccurrence = lastOccurrences.sortBy{case (d,i) => -i}.headOption

        (smallestFirstOccurrence, biggestLastOccurrence) match {
            case (Some((firstD,firstI)),Some((lastD,lastI))) => Some(FirstLastOccurrence(firstD, firstI, lastD, lastI))
            case _ => None
        }
    }

    // Part One
    val calibrationOccurrences1: Vector[FirstLastOccurrence] = calibrations.map { c => findFirstLastOccurrence(allDigitsPartOne, c) }.filter(_.isDefined).map(_.get)
    //println(calibrationOccurrences.mkString("\n"))

    val calibrationSum1: Int = calibrationOccurrences1.map(a => a.calibrationValue).sum
    println(s"Part One Calibration sum: ${calibrationSum1}")


    // Part Two
    val calibrationOccurrences2: Vector[FirstLastOccurrence] = calibrations.map { c => findFirstLastOccurrence(allDigitsPartTwo, c) }.filter(_.isDefined).map(_.get)
    //println(calibrationOccurrences.mkString("\n"))

    val calibrationSum2: Int = calibrationOccurrences2.map(a => a.calibrationValue).sum
    println(s"Part Two Calibration sum: ${calibrationSum2}")
}