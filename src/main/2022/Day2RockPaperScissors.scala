object Day2RockPaperScissors extends App {

    val strategyLines = scala.io.Source.fromFile("./Sources/Day2RockPaperScissors.txt").getLines().map(_.split(" ").toList).toList

    // Your selection score: 1 for Rock, 2 for Paper, and 3 for Scissors
    // Outcome score:   0 if you lost, 3 if the round was a draw, and 6 if you won
    def score(opp: String, you: String): Int = (opp, you) match {
        case ("A", "X") => 1 + 3
        case ("A", "Y") => 2 + 6
        case ("A", "Z") => 3 + 0

        case ("B", "X") => 1 + 0
        case ("B", "Y") => 2 + 3
        case ("B", "Z") => 3 + 6

        case ("C", "X") => 1 + 6
        case ("C", "Y") => 2 + 0
        case ("C", "Z") => 3 + 3
    }

    val scores = strategyLines.map { case a :: b :: Nil => score(a, b) }
    val scoreSum = scores.toList.sum

    println(s"Strategy total score is ${scoreSum}")


    // X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win
    def score2(opp: String, you: String): Int = (opp, you) match {
        case ("A", "X") => 3 + 0
        case ("A", "Y") => 1 + 3
        case ("A", "Z") => 2 + 6

        case ("B", "X") => 1 + 0
        case ("B", "Y") => 2 + 3
        case ("B", "Z") => 3 + 6

        case ("C", "X") => 2 + 0
        case ("C", "Y") => 3 + 3
        case ("C", "Z") => 1 + 6
    }

    val scores2 = strategyLines.map { case a :: b :: Nil => score2(a, b) }
    val score2Sum = scores2.toList.sum

    println(s"Strategy total score is ${score2Sum}")
}
