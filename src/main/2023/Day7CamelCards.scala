import scala.annotation.tailrec

object Day7CamelCards extends App {

    val handInputsRaw: Vector[String] = scala.io.Source.fromFile("./Sources/2023/Day7CamelCards.txt").getLines().toVector

    val handInputsSplit: Vector[(String, Int)] = handInputsRaw.collect{
        case s"$hand $bid" if bid.forall(_.isDigit) => (hand,bid.toInt)
    }


    // CARD
    case class Card(cardChar: Char) {
        lazy val cardStrength1 = Card.cardStrengths1.getOrElse(cardChar, -1)
        lazy val cardStrength2 = Card.cardStrengths2.getOrElse(cardChar, -1)
    }
    object Card {
        private val cardLabels1: Vector[Char] = Vector('A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2')
        private val cardLabels2: Vector[Char] = Vector('A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J')

        val cardStrengths1: Map[Char, Int] = cardLabels1.reverse.zipWithIndex.map{case(a,i) => (a,i+1)}.toMap
        val cardStrengths2: Map[Char, Int] = cardLabels2.reverse.zipWithIndex.map{case(a,i) => (a,i+1)}.toMap
    }

    val emptyCard: Card = Card('-')


    // HAND
    case class Hand(cards: Vector[Card]) {

        lazy val sameOfAKindCardGroups1: Vector[Int] = Hand.sameOfAKindCardGroups(this.cards)

        lazy val sameOfAKindCardGroups2: (Vector[Int], Int) = {
            val notJokers: Vector[Int] = Hand.sameOfAKindCardGroups(this.cards.filter(_.cardChar != 'J'))
            val jokers: Vector[Card] = this.cards.filter(_.cardChar == 'J')

            (notJokers, jokers.size)
        }

        lazy val handStrengthFromSameOfAKind1: Int = sameOfAKindCardGroups1 match {
            case Vector(5)      => 7 // Five of a kind, where all five cards have the same label: AAAAA
            case Vector(4)      => 6 // Four of a kind, where four cards have the same label and one card has a different label: AA8AA
            case Vector(2,3)    => 5 // Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
            case Vector(3)      => 4 // Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
            case Vector(2,2)    => 3 // Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
            case Vector(2)      => 2 // One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
            case Vector()       => 1 // High card, where all cards' labels are distinct: 23456
        }

        lazy val handStrengthFromSameOfAKind2: Int = sameOfAKindCardGroups2 match {
            // no jokers
            case (Vector(5)     ,0) => 7 // Five of a kind, where all five cards have the same label: AAAAA
            case (Vector(4)     ,0) => 6 // Four of a kind, where four cards have the same label and one card has a different label: AA8AA
            case (Vector(2, 3)  ,0) => 5 // Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
            case (Vector(3)     ,0) => 4 // Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
            case (Vector(2, 2)  ,0) => 3 // Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
            case (Vector(2)     ,0) => 2 // One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
            case (Vector()      ,0) => 1 // High card, where all cards' labels are distinct: 23456

            // 1 joker
            case (Vector(4),     1) => 7
            case (Vector(3),     1) => 6
            case (Vector(2,2),   1) => 5
            case (Vector(2),     1) => 4
            case (Vector(),      1) => 2

            // 2 jokers
            case (Vector(3),     2) => 7
            case (Vector(2),     2) => 6
            case (Vector(),      2) => 4

            // 3 jokers
            case (Vector(2),     3) => 7
            case (Vector(),      3) => 6

            // 4 jokers
            case (Vector(),      4) => 7

            // all jokers
            case (Vector(),      5) => 7
        }

        lazy val handStrengthFromCardLabels1: Int = Hand.labelStrength1(this.cards)
        lazy val handStrengthFromCardLabels2: Int = Hand.labelStrength2(this.cards)

        lazy val handStrength1: Int = handStrengthFromSameOfAKind1 * 1000000 + handStrengthFromCardLabels1 // max possible handStrengthFromCardLabels is below 1000000
        lazy val handStrength2: Int = handStrengthFromSameOfAKind2 * 1000000 + handStrengthFromCardLabels2
    }
    object Hand {
        // same for 1 and 2. ordered by cardStrength1, but it can be used for 2 as well
        @tailrec
        def sameOfAKindCardGroups(
                                     hand: Vector[Card],
                                     currentCard: Card = emptyCard,
                                     currentCardStackSize: Int = 1,
                                     accuSameStackSizes: Vector[Int] = Vector()
                                 ): Vector[Int] = hand.sortBy(_.cardStrength1) match {
            case Vector() =>
                (if(currentCardStackSize > 1) accuSameStackSizes :+ currentCardStackSize else accuSameStackSizes).sorted

            case c +: rest if c == currentCard =>
                sameOfAKindCardGroups(rest, currentCard, currentCardStackSize+1, accuSameStackSizes)

            case c +: rest if c != currentCard =>
                sameOfAKindCardGroups(rest, c, 1, if(currentCardStackSize > 1) accuSameStackSizes :+ currentCardStackSize else accuSameStackSizes )
        }

        @tailrec
        def labelStrength1(hand: Vector[Card], curPower: Int = 0, accu: Int = 0): Int = hand match {
            case Vector() => accu
            case rest :+ c => labelStrength1(rest, curPower + 1, accu + math.pow(14, curPower).intValue * c.cardStrength1)
        }

        @tailrec
        def labelStrength2(hand: Vector[Card], curPower: Int = 0, accu: Int = 0): Int = hand match {
            case Vector() => accu
            case rest :+ c => labelStrength2(rest, curPower + 1, accu + math.pow(14, curPower).intValue * c.cardStrength2)
        }
    }


    val parsedHandsAndBids: Vector[(Hand, Int)] = handInputsSplit.map{case(handRaw, bid) => (Hand(handRaw.toVector.map(Card(_))), bid)}

    // Part One
    val handBidRank1 = parsedHandsAndBids.sortBy { case (hand, _) => hand.handStrength1 }.zipWithIndex.map { case ((hand, bid), rank) => (hand, bid, rank+1) }
    val totalRank1 = handBidRank1.map { case(_, bid, rank) => bid * rank }.sum
    println(s"Sum of ranked Hand Bids is $totalRank1")

    // Part Two
    val handBidRank2 = parsedHandsAndBids.sortBy { case (hand, _) => hand.handStrength2 }.zipWithIndex.map { case ((hand, bid), rank) => (hand, bid, rank+1) }
    val totalRank2 = handBidRank2.map { case (_, bid, rank) => bid * rank }.sum
    println(s"Sum of ranked Hand Bids with Jokers is $totalRank2")
}