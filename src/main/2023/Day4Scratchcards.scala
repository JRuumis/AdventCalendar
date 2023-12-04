import scala.annotation.tailrec

object Day4Scratchcards extends App {
    val cardsInputRaw = scala.io.Source.fromFile("./Sources/2023/Day4Scratchcards.txt").getLines().toVector

    val cardsStack: Vector[(Int, Set[Int], Set[Int])] = cardsInputRaw.collect(_ match {
        case s"Card $cardNum:$rest" => (
            cardNum.trim.toInt,
            rest.split("""\|""")
                .toVector
                .map(_.trim.split(" ").toVector
                .collect{
                    case a if a != "" && a.forall(_.isDigit) => a.trim.toInt
                })
        )
    }).collect{ case(cardId, Vector(a,b)) => (cardId, a.toSet, b.toSet) }

    val cardsWithWins: Vector[(Int, Int)] = cardsStack.map{case(cardId, a,b) => (cardId, (a intersect b).size)}

    // Part One
    def winsToPoints(nrOfWins: Int): Int = nrOfWins match {
        case 0 => 0
        case i => math.pow(2,i-1).intValue
    }

    val winningPoints: Int = cardsWithWins.map{case(_, a) => winsToPoints(a)}.sum
    println(s"Number of winning points is $winningPoints")

    // Part Two
    @tailrec
    def cardMultiplier(cardsWithWins: Vector[(Int, Int)], multipliedCards: Vector[Int] = Vector(), totalNrOfCards: Int = 0): Int = cardsWithWins match {
        case Vector() => totalNrOfCards
        case (cardId, nrOfWins) +: rest => {
            val nrOfCurrentCards: Int = 1 + multipliedCards.filter(_ == cardId).size
            val currentCardsToMultiply: Vector[Int] = (0 to nrOfWins).toVector.filter(_ > 0).map(_ + cardId)
            val currentCardsMultiplied = (1 to nrOfCurrentCards).toVector.flatMap(_ => currentCardsToMultiply)

            cardMultiplier(rest, multipliedCards ++ currentCardsMultiplied, totalNrOfCards + nrOfCurrentCards)
        }
    }

    val totalNrOfCards: Int = cardMultiplier(cardsWithWins)
    println(s"Total number of cards after multiplication is $totalNrOfCards")
}