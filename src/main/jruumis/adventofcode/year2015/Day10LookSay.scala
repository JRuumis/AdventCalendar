package jruumis.adventofcode.year2015

object Day10LookSay extends App {

    //val lookSayInput = "1".toVector // TEST
    val lookSayInput = "1321131112".toVector // PROD

    val epochs: Int = 50

    def iterate(remainingChars: Vector[Char], currentEpoch: Int = 1, currentChar: Char = ' ', currentOccurrences: Int = 0, accuNextGen: Vector[Char] = Vector()): Vector[Char] = {
        val addCurrent: Vector[Char] = if(currentOccurrences > 0) currentOccurrences.toString.toVector :+ currentChar else Vector()

        remainingChars match {
            case Vector() if currentEpoch < epochs => iterate(accuNextGen ++ addCurrent, currentEpoch + 1)
            case Vector() if currentEpoch == epochs => accuNextGen ++ currentOccurrences.toString.toVector :+ currentChar
            case newChar +: rest if newChar == currentChar => iterate(rest, currentEpoch, currentChar, currentOccurrences+1, accuNextGen)
            case newChar +: rest if newChar != currentChar => iterate(rest, currentEpoch, newChar, 1, accuNextGen ++ addCurrent)
        }
    }

    val lookSayAfterEpochs: Vector[Char] = iterate(lookSayInput)
    println(s"""The sequence "${lookSayInput.mkString}" will be ${lookSayAfterEpochs.length} chars long after ${epochs} epochs.""")
}