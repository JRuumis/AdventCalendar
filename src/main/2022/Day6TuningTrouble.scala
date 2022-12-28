object Day6TuningTrouble extends App {

    val tuningInput: List[Char] = scala.io.Source.fromFile("./Sources/Day6TuningTrouble.txt").toList

    def tuner(stream: List[Char], currentPosition: Int, sampleSize: Int): String = stream match {
        case a if a.size >= sampleSize && a.take(sampleSize).toSet.size == sampleSize => s"First position with sample size ${sampleSize} detected at ${currentPosition}"
        case a :: rest => tuner(rest, currentPosition + 1, sampleSize)
        case _ => s"Error! Packet with sample size ${sampleSize} not detected!"
    }

    println(tuner(tuningInput, 4, 4))
    println(tuner(tuningInput, 14, 14))
}
